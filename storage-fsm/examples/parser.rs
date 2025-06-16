use std::mem;
use log::warn;
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

pub enum CsafeParserOutput {
    AddrParsed { src_addr: CsafeAddr, dst_addr: CsafeAddr },
    ShortCommand { cmd: u8 },
    LongCommand { cmd: u8, body: Vec<u8> },
    Response { status: CsafeStatus, cmd: u8, body: Vec<u8> },
    ParsingFailed{ err_msg: &'static str }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct CsafeStatus(pub u8);

state_machine! {
    name: CsafeRawParser
    input: {byte: u8}
    output_name: CsafeParserOutput
    initial_state: StartFlag
    middlewares: {
        CommandChecksum {
            chk: u8
        },
        RspChecksum {
            chk: u8
        },
        Unstuffing {
            prev_stuffed: bool,
        }
    }
    states: {
        #[reset_storage]
        StartFlag => DstAddr | Self,
        {
            middleware Unstuffing {
                // parsing started
                DstAddr => SrcAddr,
                SrcAddr {dst_addr: CsafeAddr} => Command | RspStatus,
                middleware CommandChecksum {
                    #[reset_storage]
                    Command => CommandLength | CommandChecksum,
                    CommandLength {cmd: u8} => CommandBody
                    CommandBody {body: Vec<u8>, remaining_length: u8} => CommandChecksum | Self,
                }
                CommandChecksum {collected_checksum: u8} => StopFlag,

                middleware RspChecksum {
                    #[reset_storage]
                    RspStatus => RspCommand,
                    RspCommand {status: CsafeStatus} => RspLength | RspChecksum,
                    RspLength {cmd: u8} => RspBody,
                    RspBody {body: Vec<u8>, remaining_length: u8} => RspChecksum | Self,
                }
                RspChecksum {collected_checksum: u8} => StopFlag,
            }
            #[reset_storage]
            StopFlag => StartFlag,
        } => ParsingFailure,
        inline ParsingFailure {err_msg: &'static str} => StartFlag,
    }
}

pub fn main() {
    simple_logger::init_with_level(log::Level::Debug).unwrap();
    
    let bytes = [0xF0u8, 0x01, 0x00, 0x7E, 0x01, 0x30, 0x4F, 0xF2].repeat(4);
    use CsafeParserOutput::*;
    use CsafeRawParserCmd::*;
    let mut parser = CsafeRawParser::new(|cmd| match cmd {
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
            if dst_addr != CsafeAddr::PcHost {
                // Command
                ctx.transition_command()
                    .emit(CsafeParserOutput::AddrParsed {
                        src_addr: addr,
                        dst_addr
                    })
            }
            else {
                // Response
                ctx.transition_rsp_status()
                    .emit(CsafeParserOutput::AddrParsed {
                        src_addr: addr,
                        dst_addr
                    })
            }
        }
        Command(ctx) => {
            let cmd = ctx.byte;
            // overwrite the command checksum with the current byte
            ctx.storage.command_checksum_middleware.chk = ctx.byte;
            if cmd >= 0x80 {
                let chk = ctx.storage.command_checksum_middleware.chk;
                ctx.transition_command_checksum(cmd, vec![], 0, chk)
            }
            else {
                ctx.transition_command_length(cmd)
            }
        }
        CommandLength(ctx) => {
            let length = ctx.byte;
            ctx.transition_command_body(vec![], length)
        }
        CommandBody(ctx) => {
            let byte = ctx.byte;
            *ctx.storage.remaining_length -= 1;
            ctx.storage.body.push(byte);

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
                        .emit(CsafeParserOutput::ShortCommand {
                            cmd
                        })
                }
                else {
                    let body = mem::take(ctx.storage.body);
                    ctx.transition_stop_flag()
                        .emit(CsafeParserOutput::LongCommand {
                            cmd,
                            body
                        })
                }
            }
            else {
                warn!("Checksum mismatch: expected {:02X}, got {:02X}", collected_chk, chk);
                ctx.transition_parsing_failure("Checksum mismatch")
            }
        }

        // Responses
        RspStatus(ctx) => {
            // overwrite the command checksum with the current byte
            ctx.storage.rsp_checksum_middleware.chk = ctx.byte;

            let status = CsafeStatus(ctx.byte);
            ctx.transition_rsp_command(status)
        }
        RspCommand(ctx) => {
            let cmd = ctx.byte;
            if cmd >= 0x80 {
                let chk = ctx.storage.rsp_checksum_middleware.chk;
                ctx.transition_rsp_checksum(cmd, vec![], 0, chk)
            }
            else {
                ctx.transition_rsp_length(cmd)
            }
        }
        RspLength(ctx) => {
            let length = ctx.byte;
            ctx.transition_rsp_body(vec![], length)
        }
        RspBody(ctx) => {
            let byte = ctx.byte;

            *ctx.storage.remaining_length -= 1;
            ctx.storage.body.push(byte);

            if *ctx.storage.remaining_length == 0 {
                let chk = ctx.storage.rsp_checksum_middleware.chk;
                ctx.transition_rsp_checksum(chk)
            }
            else {
                ctx.stay()
            }
        }
        RspChecksum(ctx) => {
            let chk = ctx.byte;
            let collected_chk = *ctx.storage.collected_checksum;
            if chk == collected_chk {
                let cmd = *ctx.storage.cmd;
                let body = mem::take(ctx.storage.body);
                let status = *ctx.storage.status;
                ctx.transition_stop_flag()
                    .emit(CsafeParserOutput::Response {
                        status,
                        cmd,
                        body
                    })
            }
            else {
                warn!("Checksum mismatch in the response: expected {:02X}, got {:02X}", collected_chk, chk);
                ctx.transition_parsing_failure("Checksum mismatch in the response")
            }
        }
        StopFlag(ctx) => {
            let byte = ctx.byte;
            if byte == 0xF2 {
                ctx.transition_start_flag()
            }
            else {
                ctx.transition_parsing_failure("Invalid stop flag")
            }
        }
        ParsingFailure(ctx) => {
            let err_msg = *ctx.err_msg;
            ctx.transition_start_flag()
                .emit(CsafeParserOutput::ParsingFailed {
                    err_msg
                })
        }


        // Middlewares
        UnstuffingMiddleware(ctx) => {
            let byte = *ctx.byte;
            let new_byte = if ctx.storage.prev_stuffed {
                ctx.storage.prev_stuffed = false;
                if byte > 0x03 {
                    warn!("Unstuffing middleware: invalid stuffing byte: {:02X}", byte);
                    return ctx.drop();
                }
                else {
                    let byte = byte ^ 0xF0; // unstuffing
                    byte
                }
            }
            else {
                if byte == 0xF3 {
                    // stuffing
                    ctx.storage.prev_stuffed = true;
                    return ctx.drop();
                }
                else {
                    byte
                }
            };
            *ctx.byte = new_byte;
            ctx.next()
        }
        CommandChecksumMiddleware(ctx) => {
            ctx.storage.chk ^= *ctx.byte;
            ctx.next()
        }
        RspChecksumMiddleware(ctx) => {
            ctx.storage.chk ^= *ctx.byte;
            ctx.next()
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
            Response { status, cmd, body } => {
                println!("Parsed response: status = {:?}, cmd = 0x{:02X}, body = {:?}", status, cmd, body);
            }
        }

    };

    for byte in bytes {
        parser.advance(byte, handle_output);
    }
}