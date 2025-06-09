use storage_fsm_macro::state_machine;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CsafeAddr {
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

pub enum ParserStateOutput {
    AddrParsed { src_addr: CsafeAddr, dst_addr: CsafeAddr },
    ShortCommand { cmd: u8 },
    LongCommand { cmd: u8, body: Vec<u8> },
    Response { state: CsafeState, cmd: u8, body: Vec<u8> },
    ParsingFailed{ prev_state: &'static str }
}
use ParserStateOutput::*;

state_machine! {
    name: ParserState
    input: {byte: u8}
    output_name: ParserStateOutput
    states: {
        #[reset_storage]
        StartFlag => DstAddr,
        {
            DstAddr => SrcAddr,
            SrcAddr {dst_addr: CsafeAddr} => Command | RspState,
            middleware checksum_calculation(checksum: u8) {
                Command {src_addr: CsafeAddr} => CommandLength,
                CommandLength {cmd: u8} => CommandBody
                CommandBody {body: Vec<u8>, remaining_length: u8} => Checksum | Self,
            }   
            Checksum {chk: u8} => StopFlag,
            
            RspState {src_addr: CsafeAddr} => RspCommand,
            middleware checksum_calculation(checksum: u8) {
                RspCommand {state: CsafeState} => RspLength,
                RspLength {cmd: u8} => RspBody,
                RspBody {body: Vec<u8>, remaining_length: u8} => RspChecksum | Self,
            }
            RspChecksum {chk: u8} => StopFlag,
            
            StopFlag => StartFlag,
        } => ParsingFailure,
        inline ParsingFailure {} => StartFlag,
    }
}

pub fn main() {
    let bytes = [0xF2u8, 0x00, 0xFF, 0x7E, 0x01, 0x20, 0xF3];
    let mut parser = ParserState::new();
    for byte in bytes {
        if let Some(output) = parser.advance(byte) {
            match output {
                ParsingFailed { prev_state } => {
                    println!("Parsing failed during state: {}", prev_state);
                }
                AddrParsed { src_addr, dst_addr } => {}
                ShortCommand { cmd } => {}
                LongCommand { cmd, body } => {}
                Response { state, cmd, body } => {}
            }
        }
    }
}