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

pub enum ParserStateOutput {
    AddrParsed { src_addr: CsafeAddr, dst_addr: CsafeAddr },
    ShortCommand { cmd: u8 },
    LongCommand { cmd: u8, body: Vec<u8> },
    Response { state: CsafeState, cmd: u8, body: Vec<u8> },
    ParsingFailed{ prev_state: &'static str }
}
use ParserStateOutput::*;
// state_machine! {
//     name: ParserState
//     input: {byte: u8}
//     output_name: ParserStateOutput
//     initial_state: StartFlag
//     states: {
//         #[reset_storage]
//         StartFlag => DstAddr,
//         {
//             DstAddr => SrcAddr,
//             SrcAddr {dst_addr: CsafeAddr} => Command | RspStatus,
//             middleware CommandChecksum {checksum: u8} {
//                 #[reset_storage]
//                 Command => CommandLength,
//                 CommandLength {cmd: u8} => CommandBody
//                 CommandBody {body: Vec<u8>, remaining_length: u8} => Checksum | Self,
//             }
//             Checksum {chk: u8} => StopFlag,
// 
//             #[reset_storage]
//             RspStatus => RspCommand,
//             middleware RspChecksum {checksum: u8} {
//                 RspCommand {state: CsafeState} => RspLength,
//                 RspLength {cmd: u8} => RspBody,
//                 RspBody {body: Vec<u8>, remaining_length: u8} => RspChecksum | Self,
//             }
//             RspChecksum {chk: u8} => StopFlag,
// 
//             StopFlag => StartFlag,
//         } => ParsingFailure,
//         inline ParsingFailure {prev_state: &'static str} => StartFlag,
//     }
// }

/// Expected output
#[derive(Copy, Clone)]
pub enum ParserStateKind {
    StartFlag,
    DstAddr,
    SrcAddr,
    Command,
    CommandLength,
    CommandBody,
    Checksum,
    RspStatus,
    RspCommand,
    RspLength,
    RspBody,
    RspChecksum,
    StopFlag,
}
#[derive(Copy, Clone)]
pub enum ParserStateInlineKind {
    ParsingFailure
}


// Middleware handlers
#[derive(Default)]
pub struct ChecksumMiddleware {
    chk: u8,
}

type ParserStateHandlerType = fn(parser_state_private::ParserStateCmd) -> parser_state_private::ParserStateHandlerResult;
pub struct ParserState {
    state_kind: ParserStateKind,
    handler: ParserStateHandlerType,
    state: parser_state_private::ParserStateState,
}
impl ParserState {
    pub fn new(handler: ParserStateHandlerType) -> Self {
        Self {
            state_kind: ParserStateKind::StartFlag,
            handler,
            state: Default::default(),
        }
    }
}

mod parser_state_private {
    use storage_fsm::HandlerResult;
    use crate::{ChecksumMiddleware, CsafeAddr, ParserState, ParserStateInlineKind, ParserStateKind, ParserStateOutput};
    
    #[derive(Default)]
    pub struct ParserStateState {
        dst_addr: CsafeAddr,
        src_addr: CsafeAddr,
        cmd: u8,
        body: Vec<u8>
    }

    // General things
    pub struct ParserStateHandler<S> {
        pub byte: u8,
        pub state: S,
    }
    pub struct ParserStateHandlerResult {
        handler_result: HandlerResult<ParserStateKind, ParserStateInlineKind>,
        parser_state_output: Option<ParserStateOutput>,
    }
    impl ParserStateHandlerResult {
        pub fn emit(mut self, output: ParserStateOutput) -> Self {
            self.parser_state_output = Some(output);
            self
        }
    }

    impl<S> ParserStateHandler<S> {
        pub fn stay(self) -> ParserStateHandlerResult {
            ParserStateHandlerResult {
                handler_result: HandlerResult::Stay,
                parser_state_output: None,
            }
        }
    }
    
    // State handlers
    pub struct HandleStartFlag;
    impl ParserStateHandler<HandleStartFlag> {
        pub fn transition_dst_addr(self) -> ParserStateHandlerResult {
            ParserStateHandlerResult {
                handler_result: HandlerResult::Transition(ParserStateKind::StartFlag),
                parser_state_output: None,
            }
        }
    }
    
    pub struct HandleDstAddr<'a> {
        // future
        dst_addr: &'a mut CsafeAddr
    }
    impl ParserStateHandler<HandleDstAddr<'_>> {
        pub fn transition_src_addr(self, dst_addr: CsafeAddr) -> ParserStateHandlerResult {
            *self.state.dst_addr = dst_addr;
            ParserStateHandlerResult {
                handler_result: HandlerResult::Transition(ParserStateKind::SrcAddr),
                parser_state_output: None,
            }
        }
    }

    pub struct HandleSrcAddr<'a> {
        // current
        pub dst_addr: &'a mut CsafeAddr,
    }
    impl ParserStateHandler<HandleSrcAddr<'_>> {
        pub fn transition_command(self) -> ParserStateHandlerResult {
            ParserStateHandlerResult {
                handler_result: HandlerResult::Transition(ParserStateKind::Command),
                parser_state_output: None,
            }
        }
    }
    
    // reset storage
    pub struct HandleCommand<'a> {
        // future
        cmd: &'a mut u8,
    }
    impl ParserStateHandler<HandleCommand<'_>> {
        pub fn transition_command_length(self, command: u8) -> ParserStateHandlerResult {
            *self.state.cmd = command;
            ParserStateHandlerResult {
                handler_result: HandlerResult::Transition(ParserStateKind::CommandLength),
                parser_state_output: None,
            }
        }
    }
    
    pub struct HandleCommandLength<'a> {
        // current
        pub cmd: &'a mut u8,
        // future
        command_length: &'a mut u8,
    }
    pub struct HandleCommandBody<'a> {
        // current
        pub cmd: &'a mut u8,
        pub command_length: &'a mut u8,
        // future
        command_body: Vec<u8>,
    }

    pub struct HandleCommandChecksum<'a> {
        // current
        pub cmd: &'a mut u8,
        pub command_length: &'a mut u8,
        pub command_body: Vec<u8>,
    }
    
    pub struct HandleRspStatus<'a> {
        // future
        status: &'a mut u8,
    }
    pub struct HandleRspCommand<'a> {
        // current
        pub status: &'a mut u8,
        // future
        cmd: &'a mut u8,
    }
    pub struct HandleRspLength<'a> {
        // current
        pub status: &'a mut u8,
        pub cmd: &'a mut u8,
        // future
        command_length: &'a mut u8,
    }
    pub struct HandleRspBody<'a> {
        // current
        pub status: &'a mut u8,
        pub cmd: &'a mut u8,
        pub command_length: &'a mut u8,
        // future
        command_body: Vec<u8>,
    }

    pub struct HandleRspChecksum<'a> {
        // current
        pub status: &'a mut u8,
        pub cmd: &'a mut u8,
        pub command_length: &'a mut u8,
        pub command_body: Vec<u8>,
    }
    
    pub struct HandleStopFlag;

    pub enum ParserStateCmd<'a> {
        StartFlag(ParserStateHandler<HandleStartFlag>),
        DstAddr(ParserStateHandler<HandleDstAddr<'a>>),
        SrcAddr(ParserStateHandler<HandleSrcAddr<'a>>),
        Command(ParserStateHandler<HandleCommand<'a>>),
        CommandLength(ParserStateHandler<HandleCommandLength<'a>>),
        CommandBody(ParserStateHandler<HandleCommandBody<'a>>),
        CheckSum(ParserStateHandler<HandleCommandChecksum<'a>>),
        
        RspStatus(ParserStateHandler<HandleRspStatus<'a>>),
        RspCommand(ParserStateHandler<HandleRspCommand<'a>>),
        RspLength(ParserStateHandler<HandleRspLength<'a>>),
        RspBody(ParserStateHandler<HandleRspBody<'a>>),
        RspChecksum(ParserStateHandler<HandleRspChecksum<'a>>),
        
        StopFlag(ParserStateHandler<HandleStopFlag>),

        ChecksumMiddlewareEnter {
            state: &'a mut ChecksumMiddleware,
            forward: ParserStateHandlerResult,
        },
        ChecksumMiddleware{
            state: &'a mut ChecksumMiddleware, 
            byte: u8,
            forward: ParserStateHandlerResult,
        }
    }
    
    impl ParserState {
        /// Execute inline transitions here
        fn handle_transition_result(&mut self, res: ParserStateHandlerResult) -> Option<ParserStateOutput> {
            let output = res.parser_state_output;
            match res.handler_result {
                HandlerResult::Stay => {},
                HandlerResult::Transition(k) => {
                    self.state_kind = k;
                }
                HandlerResult::TransitionInline(i) => {
                    // execute inline handler
                }
            }
            output
        }
        pub fn advance(&mut self, byte: u8) -> Option<ParserStateOutput> {
            match self.state_kind {
                ParserStateKind::StartFlag => {
                    let res = (self.handler)(ParserStateCmd::StartFlag(ParserStateHandler::<HandleStartFlag> {
                        byte,
                        state: HandleStartFlag,
                    }));
                    self.handle_transition_result(res)
                }
                ParserStateKind::DstAddr => {
                    let res = (self.handler)(ParserStateCmd::DstAddr(ParserStateHandler::<HandleDstAddr> {
                        byte,
                        state: HandleDstAddr {
                            dst_addr: &mut self.state.dst_addr
                        },
                    }));

                    self.handle_transition_result(res)
                }
                ParserStateKind::Command => {
                    let res = (self.handler)(ParserStateCmd::Command(ParserStateHandler::<HandleCommand> {
                        byte,
                        state: HandleCommand {
                            cmd: &mut self.state.cmd
                        }
                    }));
                    
                    self.handle_transition_result(res)
                }
                _ => {None}
            }
        }
        
    }
}
use parser_state_private::*;

pub fn main() {
    let bytes = [0xF2u8, 0x00, 0xFF, 0x7E, 0x01, 0x20, 0xF3];
    
    let mut parser = ParserState::new(|cmd| {
       match cmd {
           // Commands
           ParserStateCmd::StartFlag(ctx) => {
               if ctx.byte == 0xF0 {
                   ctx.transition_dst_addr()
               }
               else {
                   ctx.stay()
               }
           }
           ParserStateCmd::DstAddr(ctx) => {
               let addr = CsafeAddr::decode(ctx.byte);
               ctx.transition_src_addr(addr)
           }
           ParserStateCmd::SrcAddr(ctx) => {
               let addr = CsafeAddr::decode(ctx.byte);
               let dst_addr = *ctx.state.dst_addr;
               ctx.transition_command().emit(AddrParsed { 
                   src_addr: addr,
                   dst_addr
               })
           }
           
           // Middlewares
           ParserStateCmd::ChecksumMiddlewareEnter {state, forward } => {
               state.chk = 0;
               forward
           }
           ParserStateCmd::ChecksumMiddleware {state, byte, forward} => {
               state.chk ^= byte;
               forward
           }
           _ => todo!()
       }
    });
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