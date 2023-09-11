use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Error};

#[proc_macro_derive(ColumnDebug)]
pub fn derive_column_debug(tokens: TokenStream) -> TokenStream {
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
                                        .field(stringify!(#field_names), &#field_names.column_dbg::<#type_name>())
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
                impl ::apheleia_prism::ColumnDebug for #type_name {
                    fn fmt<Col>(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
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
                "ColumnDebug derive is only valid on enums",
            )
            .to_compile_error()
            .into()
        }
    };
    output.into()
}
