pub struct Pat<'a> {
    pub inner: PatInner<'a>,
}

pub enum PatInner<'a> {
    Or(&'a Pat<'a>, &'a Pat<'a>),
    Wild,
}
