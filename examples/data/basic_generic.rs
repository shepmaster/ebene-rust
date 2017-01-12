trait Foo {}

fn bar<T>(t: T)
    where T: Foo,
{}
