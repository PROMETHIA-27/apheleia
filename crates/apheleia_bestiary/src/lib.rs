use std::collections::HashMap;

use apheleia_prism::prelude::*;

#[derive(Resource)]
pub struct BuiltinTypes {
    pub types: HashMap<String, Entity>,
}

/// The namespace and name that uniquely identifies an item. Modules do not have qualified names,
/// they are namespaces themselves.
#[derive(Clone, Component, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct QualifiedName {
    pub namespace: String,
    pub name: String,
}

impl QualifiedName {
    pub fn short(name: impl Into<String>) -> Self {
        Self {
            namespace: String::new(),
            name: name.into(),
        }
    }
}

pub fn create_builtin_types(c: &mut Commands) -> HashMap<QualifiedName, Entity> {
    let mut scope = HashMap::new();
    scope.insert(
        QualifiedName::short("String"),
        c.spawn(TypeInfo {
            name: "String".into(),
            kind: TypeKind::Buffer { bits: 64 },
        })
        .id(),
    );
    scope.insert(
        QualifiedName::short("Char"),
        c.spawn(TypeInfo {
            name: "Char".into(),
            kind: TypeKind::Buffer { bits: 8 },
        })
        .id(),
    );
    scope.insert(
        QualifiedName::short("()"),
        c.spawn(TypeInfo {
            name: "()".into(),
            kind: TypeKind::Buffer { bits: 0 },
        })
        .id(),
    );

    c.insert_resource(BuiltinTypes {
        types: scope
            .iter()
            .map(|(name, ty)| (name.name.clone(), *ty))
            .collect(),
    });

    scope
}

#[derive(Component)]
pub struct TypeInfo {
    pub name: String,
    pub kind: TypeKind,
}

pub enum TypeKind {
    Buffer { bits: u16 },
}
