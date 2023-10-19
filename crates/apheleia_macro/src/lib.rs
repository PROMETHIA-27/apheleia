use proc_macro::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, DeriveInput, Error, Ident, ItemImpl, Token, Type};

#[proc_macro_derive(EcsTreeDebug)]
pub fn derive_ecs_tree_debug(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    let output = match input.data {
        syn::Data::Enum(data) => {
            let type_name = input.ident;
            let variant_branches: Vec<_> = data
                .variants
                .iter()
                .map(|variant| {
                    let variant_name = &variant.ident;
                    match &variant.fields {
                        syn::Fields::Named(named) => {
                            let field_names: Vec<_> =
                                named.named.iter().map(|field| &field.ident).collect();
                            quote! {
                                #type_name::#variant_name { #(#field_names,)* } => f
                                    .debug_struct(stringify!(#variant_name))
                                    #(
                                        .field(stringify!(#field_names), &#field_names.component_dbg::<#type_name>(world))
                                    )*
                                    .finish(),
                            }
                        }
                        syn::Fields::Unnamed(_) => todo!(),
                        syn::Fields::Unit => quote! {
                            #type_name::#variant_name =>
                                f.debug_struct(stringify!(#variant_name)).finish(),
                        },
                    }
                })
                .collect();
            quote! {
                impl ::apheleia_prism::EcsTreeDebug for #type_name {
                    fn fmt<C>(&self, world: &World, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        match self {
                            #(#variant_branches)*
                        }
                    }
                }
            }
        }
        _ => {
            return Error::new(
                Span::call_site().into(),
                "EcsTreeDebug derive is only valid on enums",
            )
            .to_compile_error()
            .into()
        }
    };
    output.into()
}

#[proc_macro_derive(Visit)]
pub fn derive_visit(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    let data = match input.data {
        syn::Data::Struct(data) => data,
        _ => unimplemented!(),
    };

    let children: Vec<proc_macro2::TokenStream> = data
        .fields
        .into_iter()
        .map(|field| {
            let field_name = field.ident.unwrap();
            match &field.ty.to_token_stream().to_string().replace(' ', "")[..] {
                "Entity" => quote! {
                    f(self.#field_name);
                },
                "Vec<Entity>" | "Punctuated<Entity>" => quote! {
                    for &child in self.#field_name.iter() {
                        f(child);
                    }
                },
                "Option<(Span,Entity)>" => quote! {
                    self.#field_name.as_ref().map(|(_, child)| f(*child));
                },
                "Option<(Span,Punctuated<Entity>,Span)>" => quote! {
                    self.#field_name.as_ref().map(|(_, children, _)| for &child in children.iter() {
                        f(child);
                    });
                },
                "Option<Entity>" => quote! {
                    self.#field_name.as_ref().map(|child| f(*child));
                },
                other if other.contains("Entity") => quote! {
                    compile_error!("Forgot to account for children. Update the visit macro!");
                },
                _ => quote! {},
            }
        })
        .collect();

    quote! {
        impl Visit for #name {
            fn visit(&self, mut f: impl FnMut(Entity)) {
                #(#children)*
            }
        }
    }
    .into()
}

#[proc_macro_derive(Dispatch)]
pub fn derive_dispatch(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let data = match input.data {
        syn::Data::Enum(data) => data,
        _ => unimplemented!(),
    };

    let name = input.ident;

    let nodes: Vec<Ident> = data
        .variants
        .into_iter()
        .map(|variant| variant.ident)
        .collect();

    quote! {
        impl<Op: Operation> Dispatch<Op> for #name
        where
            #(
                #nodes: Operate<Op>,
            )*
        {
            fn dispatch_impl<'args, 'world: 'args, 'rest: 'args>(entity: Entity, world: &'world World, args: Op::Arguments<'args, 'world, 'rest>) {
                let kind = world.get::<#name>(entity).unwrap();
                match kind {
                    #(
                        #name::#nodes => {
                            let node = world.get::<#nodes>(entity).unwrap();
                            node.operate(entity, world, args);
                        }
                    )*
                }
            }
        }

        #(
            impl<'node> From<&'node #nodes> for #name {
                fn from(node: &'node #nodes) -> Self {
                    #name::#nodes
                }
            }
        )*

    }
    .into()
}

/// When many nodes share the exact same implementation, this macro can be helpful. This is often
/// the case for simple nodes such as literal values, or expressions.
#[proc_macro_attribute]
pub fn impl_for_all(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemImpl);

    let types: Punctuated<Type, Token![,]> =
        Punctuated::parse_separated_nonempty.parse(attrs).unwrap();

    let impls = types
        .into_iter()
        .map(|ty| {
            let mut next = input.clone();
            next.self_ty = Box::new(ty);
            next
        })
        .collect::<Vec<_>>();

    quote! {
        #(#impls)*
    }
    .into()
}
