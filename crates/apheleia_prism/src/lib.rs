use std::marker::PhantomData;

pub mod prelude {
    pub use apheleia_macro::ColumnDebug;
    pub use bevy_ecs;
    pub use bevy_ecs::component::Component;
    pub use bevy_ecs::component::TableStorage;
    pub use bevy_ecs::entity::Entity;
    pub use bevy_ecs::query::Without;
    pub use bevy_ecs::query::{QueryState, WorldQuery};
    pub use bevy_ecs::system::Commands;
    pub use bevy_ecs::world::World;
}

pub struct DebugAsColumn<T, Col>(T, PhantomData<Col>);

// impl<Col: Column + ColumnDebug, T: ColumnDebug> Debug for DebugAsColumn<T, Col> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         T::fmt::<Col>(&self.0, f)
//     }
// }

// pub trait ColumnDebug {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug;

//     fn column_dbg<Col>(&self) -> DebugAsColumn<&Self, Col> {
//         DebugAsColumn(self, PhantomData)
//     }
// }

// impl<T: ColumnDebug> ColumnDebug for &T {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         T::fmt::<Col>(self, f)
//     }
// }

// impl ColumnDebug for Row {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         self.on_column(|col: &Col| col.fmt::<Col>(f))
//     }
// }

// impl ColumnDebug for char {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         <Self as Debug>::fmt(self, f)
//     }
// }

// impl ColumnDebug for String {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         <Self as Debug>::fmt(self, f)
//     }
// }

// impl<T: ColumnDebug> ColumnDebug for Vec<T> {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         let mut list = f.debug_list();
//         for item in self {
//             list.entry(&item.column_dbg::<Col>());
//         }
//         list.finish()
//     }
// }

// impl ColumnDebug for std::ops::Range<usize> {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         <Self as Debug>::fmt(self, f)
//     }
// }

// impl<T: ColumnDebug, U: ColumnDebug> ColumnDebug for (T, U) {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         f.write_fmt(format_args!(
//             "({:?}, {:?})",
//             &self.0.column_dbg::<Col>(),
//             &self.1.column_dbg::<Col>()
//         ))
//     }
// }

// impl<T: ColumnDebug, U: ColumnDebug, V: ColumnDebug> ColumnDebug for (T, U, V) {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         f.write_fmt(format_args!(
//             "({:?}, {:?}, {:?})",
//             &self.0.column_dbg::<Col>(),
//             &self.1.column_dbg::<Col>(),
//             &self.2.column_dbg::<Col>(),
//         ))
//     }
// }

// impl<T: ColumnDebug> ColumnDebug for Option<T> {
//     fn fmt<Col>(&self, f: &mut Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         match self {
//             Some(value) => f
//                 .debug_tuple("Some")
//                 .field(&value.column_dbg::<Col>())
//                 .finish(),
//             None => f.write_str("None"),
//         }
//     }
// }

#[macro_export]
macro_rules! ir_tree {
    (
        #[from($prev_ir:ident)]
        $(
            #[$($attrs:meta)*]
        )*
        $vis:vis enum $name:ident {
            unchanged {
                $(
                    $old_variant:ident {
                        $(
                            $old_field_name:ident : $old_field_ty:ty,
                        )*
                    },
                )*
            }
            new {
                $(
                    $new_variant:ident {
                        $(
                            $new_field_name:ident : $new_field_ty:ty,
                        )*
                    },
                )*
            }
        }
    ) => {
        $(
            #[$($attrs)*]
        )*
        $vis enum $name {
            $(
                $old_variant {
                    $(
                        $old_field_name: $old_field_ty,
                    )*
                },
            )*
            $(
                $new_variant {
                    $(
                        $new_field_name: $new_field_ty,
                    )*
                },
            )*
        }

        impl<'ir> From<&'ir $prev_ir> for $name {
            fn from(prev: &'ir $prev_ir) -> Self {
                match prev {
                    $(
                        $prev_ir::$old_variant {
                            $($old_field_name,)*
                        } => {
                            $name::$old_variant {
                                $($old_field_name: $old_field_name.clone(),)*
                            }
                        },
                    )*
                    _ => unreachable!("{} node could not be converted into a {} node automatically",
                        ::std::stringify!($prev_ir),
                        ::std::stringify!($name),
                    ),
                }
            }
        }
    };

    (@check_unchanged unchanged) => {};
}
