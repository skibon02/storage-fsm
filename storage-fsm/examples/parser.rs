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
    ParsingFailed{ prev_state: &'static str }
}
state_machine! {
    name: CsafeParser
    input: {byte: u8}
    output_name: CsafeParserOutput
    initial_state: StartFlag
    middlewares: {
        CommandChecksum {
            checksum: u8
        },
        RspChecksum {
            checksum: u8
        }
    }
    states: {
        #[reset_storage]
        StartFlag => DstAddr,
        {
            // parsing started
            DstAddr => SrcAddr,
            SrcAddr {dst_addr: CsafeAddr} => Command | RspStatus,
            middleware CommandChecksum {
                #[reset_storage]
                Command => CommandLength | CommandChecksum,
                CommandLength {cmd: u8} => CommandBody
                CommandBody {body: Vec<u8>, remaining_length: u8} => CommandChecksum | Self,
            }
            CommandChecksum {command_checksum: CommandChecksum} => StopFlag,

            #[reset_storage]
            RspStatus => RspCommand,
            middleware RspChecksum {
                RspCommand {state: CsafeState} => RspLength | RspChecksum,
                RspLength {cmd: u8} => RspBody,
                RspBody {body: Vec<u8>, remaining_length: u8} => RspChecksum | Self,
            }
            RspChecksum {command_checksum: RspChecksum} => StopFlag,

            #[reset_storage]
            StopFlag => StartFlag,
        } => ParsingFailure,
        inline ParsingFailure {err_msg: &'static str} => StartFlag,
    }
}

pub fn main() {
    let bytes = [0xF2u8, 0x00, 0xFF, 0x7E, 0x01, 0x20, 0xF3];

    use CsafeParserOutput::*;
    let mut parser = CsafeParser::new(|cmd| {
       match cmd {
           // // Commands
           // CsafeParserCmd::StartFlag(ctx) => {
           //     if ctx.byte == 0xF0 {
           //         ctx.transition_dst_addr()
           //     }
           //     else {
           //         ctx.stay()
           //     }
           // }
           // CsafeParserCmd::DstAddr(ctx) => {
           //     let addr = CsafeAddr::decode(ctx.byte);
           //     ctx.transition_src_addr(addr)
           // }
           // CsafeParserCmd::SrcAddr(ctx) => {
           //     let addr = CsafeAddr::decode(ctx.byte);
           //     let dst_addr = *ctx.storage.dst_addr;
           //     ctx.transition_command().emit(AddrParsed { 
           //         src_addr: addr,
           //         dst_addr
           //     })
           // }
           // 
           // // Middlewares
           // CsafeParserCmd::CommandChecksumMiddleware(ctx) => {
           //     ctx.storage.chk ^= ctx.byte;
           //     ctx.stay()
           // }
           // CsafeParserCmd::RspChecksumMiddleware(ctx) => {
           //     ctx.storage.chk ^= ctx.byte;
           //     ctx.stay()
           // }
           _ => todo!()
       }
    });
    let handle_output = |output| {
        match output {
            ParsingFailed { prev_state } => {
                println!("Parsing failed during state: {}", prev_state);
            }
            AddrParsed { src_addr, dst_addr } => {}
            ShortCommand { cmd } => {}
            LongCommand { cmd, body } => {}
            Response { state, cmd, body } => {}
        }

    };

    for byte in bytes {
        parser.advance(byte, handle_output);
    }
}