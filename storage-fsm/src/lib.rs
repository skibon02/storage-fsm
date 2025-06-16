pub enum HandlerResult<K, I> {
    Drop,
    Stay,
    Transition(K),
    InlineTransition(I),
}

impl<K, I> HandlerResult<K, I> {
    pub fn is_drop(&self) -> bool {
        matches!(self, HandlerResult::Drop)
    }
}