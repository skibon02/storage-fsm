pub enum HandlerResult<K, I> {
    Stay,
    Transition(K),
    TransitionInline(I),
}