pub enum HandlerResult<K, I> {
    Stay,
    Transition(K),
    InlineTransition(I),
}