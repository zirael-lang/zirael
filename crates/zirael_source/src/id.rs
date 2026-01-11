#[macro_export]
macro_rules! new_id {
  ($name:ident) => {
    $crate::paste::paste! {
      pub static [<LAST_ID_$name:upper>]: ::std::sync::atomic::AtomicUsize =
                ::std::sync::atomic::AtomicUsize::new(1);

      #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
      pub struct $name(pub usize);

      impl $name {
        pub fn new() -> Self {
          use ::std::sync::atomic::Ordering;
          Self([<LAST_ID_$name:upper>].fetch_add(1, Ordering::Relaxed) + 1)
        }

        pub fn dummy() -> Self {
          Self(usize::MAX - 1)
        }
      }
    }
  };
}
