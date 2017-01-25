//#![feature(proc_macro)]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(Visit, attributes(visit))]
pub fn visit_derive(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).expect("Unable to parse input");

    // Build the impl
    let gen = impl_visit(&ast);

    // Return the generated impl
    gen.parse().expect("Unable to generate")
}

fn camelcase_to_snake_case(camelcase: &str) -> String {
    let mut s = String::new();

    for c in camelcase.chars() {
        if c.is_lowercase() {
            s.push(c);
        } else {
            s.push('_');
            s.extend(c.to_lowercase());
        }
    }

    s
}

fn impl_visit(ast: &syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;
    let method_name_base = camelcase_to_snake_case(&name.to_string());
    let method_name: quote::Ident = format!("visit{}", method_name_base).into();
    let exit_method_name: quote::Ident = format!("exit{}", method_name_base).into();

    let visit_fields = impl_visit_fields(ast);

    quote! {
        impl Visit for #name {
            fn visit<V>(&self, v: &mut V)
                where V: Visitor
            {
                v.#method_name(self);
                #visit_fields;
                v.#exit_method_name(self);
            }
        }
    }
}

fn impl_visit_fields(ast: &syn::MacroInput) -> quote::Tokens {
    use syn::{Body, VariantData};

    match ast.body {
        Body::Enum(ref e) => {
            let enum_name = &ast.ident;

            let mut q = quote! {};

            q.append_all(e.iter().map(|variant| {
                let variant_name = &variant.ident;
                quote! { #enum_name::#variant_name(ref x) => x.visit(v), }
            }));

            quote! {
                match *self {
                    #q
                }
            }
        }
        Body::Struct(VariantData::Struct(ref fields)) |
        Body::Struct(VariantData::Tuple(ref fields)) => {
            let mut q = quote! {};
            q.append_all(fields.iter().enumerate().filter(|&(_, ref f)| !is_ignore_field(f)).map(|(i, f)| {
                let field_name: syn::Ident = f.ident.clone().unwrap_or_else(|| i.into());
                quote! { self.#field_name.visit(v); }
            }));
            q
        }
        Body::Struct(VariantData::Unit) => quote! {},
    }
}

fn is_ignore_field(field: &syn::Field) -> bool {
    use syn::MetaItem;

    let attr_name: syn::Ident = "visit".into();

    field.attrs.iter().any(|attr| {
        match attr.value {
            MetaItem::List(ref name, ref children) => {
                name == &attr_name && children.iter().any(ignore_field_inner)
            },
            _ => false,
        }
    })
}

fn ignore_field_inner(item: &syn::NestedMetaItem) -> bool {
    use syn::{NestedMetaItem, MetaItem};

    let ignore_value: syn::Ident = "ignore".into();

    match *item {
        NestedMetaItem::MetaItem(MetaItem::Word(ref i)) => i == &ignore_value,
        _ => false
    }
}
