use std::mem;
use storage_fsm_macro::state_machine;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub enum CsafeAddr {
    #[default]
    PcHost,
    Device(u8),
    DefaultSecondary,
    Reserved,
    Brodcast,
}

impl CsafeAddr {
    pub fn encode(self) -> u8 {
        match self {
            CsafeAddr::PcHost => 0x00,
            CsafeAddr::Device(addr) => addr,
            CsafeAddr::DefaultSecondary => 0xFD,
            CsafeAddr::Reserved => 0xFE,
            CsafeAddr::Brodcast => 0xFF,
        }
    }

    pub fn decode(byte: u8) -> CsafeAddr {
        match byte {
            0x00 => CsafeAddr::PcHost,
            0xFD => CsafeAddr::DefaultSecondary,
            0xFE => CsafeAddr::Reserved,
            0xFF => CsafeAddr::Brodcast,
            _ => CsafeAddr::Device(byte),
        }
    }
}

pub type CsafeState = u8;

pub enum CsafeParserOutput {
    AddrParsed { src_addr: CsafeAddr, dst_addr: CsafeAddr },
    ShortCommand { cmd: u8 },
    LongCommand { cmd: u8, body: Vec<u8> },
    Response { state: CsafeState, cmd: u8, body: Vec<u8> },
    ParsingFailed{ err_msg: &'static str }
}
state_machine! {
    name: CsafeParser
    input: {byte: u8}
    output_name: CsafeParserOutput
    initial_state: StartFlag
    middlewares: {
        CommandChecksum {
            chk: u8
        },
        RspChecksum {
            chk: u8
        }
    }
    states: {
        #[reset_storage]
        StartFlag => DstAddr | Self,
        {
            // parsing started
            DstAddr => SrcAddr,
            SrcAddr {dst_addr: CsafeAddr} => Command | RspStatus,
            middleware CommandChecksum {
                #[reset_storage]
                Command => CommandLength | CommandChecksum,
                CommandLength {cmd: u8} => CommandBody
                CommandBody {body: Vec<u8>, remaining_length: u8, is_stuffing: bool} => CommandChecksum | Self,
            }
            CommandChecksum {collected_checksum: u8} => StopFlag,

            #[reset_storage]
            RspStatus => RspCommand,
            middleware RspChecksum {
                RspCommand {state: CsafeState} => RspLength | RspChecksum,
                RspLength {cmd: u8} => RspBody,
                RspBody {body: Vec<u8>, remaining_length: u8, is_stuffing: bool} => RspChecksum | Self,
            }
            RspChecksum {collected_checksum: u8} => StopFlag,

            #[reset_storage]
            StopFlag => StartFlag,
        } => ParsingFailure,
        inline ParsingFailure {err_msg: &'static str} => StartFlag,
    }
}

pub fn main() {
    let bytes = [0xF0u8, 0x00, 0xFF, 0x7E, 0x01, 0x30, 0x4F, 0xF3].repeat(4);
    use CsafeParserOutput::*;
    use CsafeParserCmd::*;
    let mut parser = CsafeParser::new(|cmd| {
        match cmd {
            // // Commands
            StartFlag(ctx) => {
                if ctx.byte == 0xF0 {
                    ctx.transition_dst_addr()
                }
                else {
                    ctx.stay()
                }
            }
            DstAddr(ctx) => {
                let addr = CsafeAddr::decode(ctx.byte);
                ctx.transition_src_addr(addr)
            }
            SrcAddr(ctx) => {
                let addr = CsafeAddr::decode(ctx.byte);
                let dst_addr = *ctx.storage.dst_addr;
                ctx.transition_command()
                    .emit(AddrParsed {
                        src_addr: addr,
                        dst_addr
                    })
            }
            Command(ctx) => {
                let cmd = ctx.byte;
                // overwrite the command checksum with the current byte
                ctx.storage.command_checksum_middleware.chk = ctx.byte;
                if cmd >= 0x80 {
                    let chk = ctx.storage.command_checksum_middleware.chk;
                    ctx.transition_command_checksum(chk)
                }
                else {
                    ctx.transition_command_length(cmd)
                }
            }
            CommandLength(ctx) => {
                let length = ctx.byte;
                ctx.transition_command_body(vec![], false, length)
            }
            CommandBody(ctx) => {
                let byte = ctx.byte;
                let new_byte = if *ctx.storage.is_stuffing {
                    if byte > 0x03 {
                        return ctx.transition_parsing_failure("Invalid stuffing byte")
                    }
                    else {
                        let byte = byte ^ 0xF0; // unstuffing
                        byte
                    }
                }
                else {
                    if byte == 0xF3 {
                        // stuffing
                        *ctx.storage.is_stuffing = true;
                        return ctx.stay();
                    }
                    else {
                        byte
                    }
                };

                *ctx.storage.remaining_length -= 1;
                ctx.storage.body.push(new_byte);

                if *ctx.storage.remaining_length == 0 {
                    let chk = ctx.storage.command_checksum_middleware.chk;
                    ctx.transition_command_checksum(chk)
                }
                else {
                    ctx.stay()
                }
            }
            CommandChecksum(ctx) => {
                let chk = ctx.byte;
                let collected_chk = *ctx.storage.collected_checksum;
                if chk == collected_chk {
                    let cmd = *ctx.storage.cmd;
                    if cmd >= 0x80 {
                        ctx.transition_stop_flag()
                            .emit(ShortCommand {
                                cmd
                            })
                    }
                    else {
                        let body = mem::take(ctx.storage.body);
                        ctx.transition_stop_flag()
                            .emit(LongCommand {
                                cmd,
                                body
                            })
                    }
                }
                else {
                    ctx.transition_parsing_failure("Checksum mismatch")
                }
            }
            StopFlag(ctx) => {
                let byte = ctx.byte;
                if byte == 0xF3 {
                    ctx.transition_start_flag()
                }
                else {
                    ctx.transition_parsing_failure("Invalid stop flag")
                }
            }
            ParsingFailure(ctx) => {
                let err_msg = *ctx.err_msg;
                ctx.transition_start_flag()
                    .emit(ParsingFailed {
                        err_msg
                    })
            }

            // Middlewares
            CommandChecksumMiddleware(mut ctx) => {
                ctx.storage.chk ^= ctx.byte;
                ctx.next()
            }
            RspChecksumMiddleware(mut ctx) => {
                ctx.storage.chk ^= ctx.byte;
                ctx.next()
            }
            _ => todo!()
        }
    });
    let handle_output = |output| {
        match output {
            ParsingFailed { err_msg } => {
                println!("Parsing failed during state: {}", err_msg);
            }
            AddrParsed { src_addr, dst_addr } => {
                println!("Parsed addresses: src = {:?}, dst = {:?}", src_addr, dst_addr);
            }
            ShortCommand { cmd } => {
                println!("Parsed short command: 0x{:02X}", cmd);
            }
            LongCommand { cmd, body } => {
                println!("Parsed long command: 0x{:02X}, body = {:?}", cmd, body);
            }
            Response { state, cmd, body } => {
                println!("Parsed response: state = {}, cmd = 0x{:02X}, body = {:?}", state, cmd, body);
            }
        }

    };

    for byte in bytes {
        parser.advance(byte, handle_output);
    }
}