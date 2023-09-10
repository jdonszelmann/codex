
#[macro_export]
macro_rules! root {
    (
        $parent:ident@$name: literal: [$($dir:tt)*], $($rest: tt)*
    ) => {
        let dir_builder = $parent.create_dir($name);
        let original_dir = root!(dir_builder@$($dir)*).build();
        root!(original_dir@$($rest)*)
    };
    (
        $parent:ident@$name: ident: [$($dir:tt)*], $($rest: tt)*
    ) => {
        {
            let dir_builder = $parent.create_dir(stringify!($name));
            let original_dir = root!(dir_builder@$($dir)*).build();
            root!(original_dir@$($rest)*)
        }
    };
    (
        $parent:ident@#$name: ident: [$($dir:tt)*], $($rest: tt)*
    ) => {
        {
            let dir_builder = $parent.create_dir(stringify!($name));
            let original_dir = root!(dir_builder@$($dir)*).build();
            root!(original_dir@$($rest)*)
        }
    };
    (
        $parent:ident@$name: literal: $contents: literal, $($rest: tt)*
    ) => {
        {
            let res = $parent.create_file($name, $contents);
            root!(res@$($rest)*)
        }
    };
    (
        $parent:ident@$name: ident: $contents: literal, $($rest: tt)*
    ) => {
        a
    };
    (
        $parent:ident@#$name: ident: $contents: literal, $($rest: tt)*
    ) => {
        b
    };

    (
        $parent:ident@$name: literal: #$contents: ident, $($rest: tt)*
    ) => {
        {
            let dir = $parent.create_file($name, $contents);
            root!(dir@$($rest)*)
        }
    };
    (
        $parent:ident@$name: ident: #$contents: ident, $($rest: tt)*
    ) => {
        c
    };
    (
        $parent:ident@#$name: ident: #$contents: ident, $($rest: tt)*
    ) => {
        d
    };

    (
        $name: literal: [$($tt: tt)*]
    ) => {
        {
            res = $crate::Root::new($name);
            root!(res@$($tt)*).build()
        }
    };
    (
        $name: ident: [$($tt: tt)*] $(,)?
    ) => {
        {
            let res = $crate::Root::new_in_memory(stringify!($name));
            root!(res @ $($tt)*).build()
        }
    };
    (
        #$name: ident: [$($tt: tt)*] $(,)?
    ) => {
        {
            let res = $crate::Root::new_in_memory($name);
            root!(res @ $($tt)*).build();
        }
    };
    () => {};
    ($parent: ident@) => { $parent };
}
