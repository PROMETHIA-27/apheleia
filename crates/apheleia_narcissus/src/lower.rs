use std::collections::HashMap;

use apheleia_bestiary::{create_builtin_types, QualifiedName};
use apheleia_mandrake::{
    AttributeList, CharLiteral, Cst, ExternalFunc, FieldInitializer, FunctionArg, FunctionDef,
    Invalid, MemberAccess, Module, NameAccess, RecordConstructor, StringLiteral, Type,
};
use apheleia_prism::prelude::*;

use crate::{
    FunctionCall, Hir, TypedExternalFunc, TypedFunctionArg, TypedFunctionDef,
    TypedRecordConstructor,
};

pub fn lower_cst(mut c: Commands, world: &World, roots: Query<Entity, With<RootNode>>) {
    let types = create_builtin_types(&mut c);
    let unit = *types.get(&QualifiedName::short("()")).unwrap();
    let mut args = LowerArgs {
        stack: vec![types],
        commands: c,
        unit_type: unit,
    };
    for root in &roots {
        Cst::dispatch::<LowerCstToHir>(root, world, &mut args);
    }
}

struct LowerCstToHir;

impl Operation for LowerCstToHir {
    type Arguments<'args, 'world: 'args, 'x: 'args> = &'args mut LowerArgs<'x>;
}

struct LowerArgs<'x> {
    commands: Commands<'x, 'x>,
    stack: Vec<HashMap<QualifiedName, Entity>>,
    unit_type: Entity,
}

impl LowerArgs<'_> {
    fn push_scope_from_items<'item, I>(&mut self, items: I, world: &World)
    where
        I: Iterator<Item = &'item Entity> + Clone,
    {
        self.stack.push(
            items
                .clone()
                .map(get_item_kv_by_full_name(world))
                .chain(items.map(get_item_kv_by_short_name(world)))
                .collect(),
        )
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn walk_for(&mut self, name: &QualifiedName) -> Option<Entity> {
        for scope in self.stack.iter().rev() {
            if let Some(node) = scope.get(name) {
                return Some(*node);
            }
        }
        None
    }

    #[allow(dead_code)]
    fn dump_stack(&self) {
        println!("Scope stack:");
        for scope in &self.stack {
            println!("-------\n{scope:#?}");
        }
    }
}

fn get_item_kv_by_full_name(world: &World) -> impl Fn(&Entity) -> (QualifiedName, Entity) + '_ {
    |item| (world.get::<QualifiedName>(*item).unwrap().clone(), *item)
}

fn get_item_kv_by_short_name(world: &World) -> impl Fn(&Entity) -> (QualifiedName, Entity) + '_ {
    |item| {
        let full_name = world.get::<QualifiedName>(*item).unwrap();
        (
            QualifiedName {
                namespace: String::new(),
                name: full_name.name.clone(),
            },
            *item,
        )
    }
}

impl Operate<LowerCstToHir> for Module {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        args.commands.entity(entity).insert(Hir::Module);

        for &attr in &self.attributes {
            Cst::dispatch::<LowerCstToHir>(attr, world, args);
        }

        args.push_scope_from_items(self.items.iter(), world);

        for &item in &self.items {
            Cst::dispatch::<LowerCstToHir>(item, world, args);
        }

        args.pop_scope();
    }
}

impl Operate<LowerCstToHir> for FunctionDef {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let attributes = self.attributes.clone();
        let name = self.name.1.clone();
        let func_args = self.args.to_vec();

        let return_ty = self
            .return_ty
            .as_ref()
            .map(|(_, ty)| {
                let ty = world.get::<Type>(*ty).unwrap();
                args.walk_for(&QualifiedName::short(&ty.name.1)).unwrap()
            })
            .unwrap_or(args.unit_type);

        let body = self.body.clone();
        let items = self.items.clone();

        args.commands.entity(entity).insert((
            Hir::TypedFunctionDef,
            TypedFunctionDef {
                attributes,
                name,
                args: func_args,
                return_ty,
                body,
                items,
            },
        ));

        for &node in self.attributes.iter().chain(self.args.iter()) {
            Cst::dispatch::<LowerCstToHir>(node, world, args);
        }

        args.push_scope_from_items(self.items.iter(), world);

        for &node in self.body.iter().chain(self.items.iter()) {
            Cst::dispatch::<LowerCstToHir>(node, world, args);
        }

        args.pop_scope();
    }
}

impl Operate<LowerCstToHir> for FunctionArg {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let attributes = self.attributes.clone();
        let name = self.name.1.clone();

        let ty = {
            let ty = world.get::<Type>(self.ty).unwrap();
            args.walk_for(&QualifiedName::short(&ty.name.1)).unwrap()
        };

        args.commands.entity(entity).insert((
            Hir::TypedFunctionArg,
            TypedFunctionArg {
                attributes,
                name,
                ty,
            },
        ));
    }
}

impl Operate<LowerCstToHir> for RecordConstructor {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let ty = {
            let ty = world.get::<Type>(self.ty).unwrap();
            args.walk_for(&QualifiedName::short(&ty.name.1)).unwrap()
        };

        let fields = self.fields.to_vec();
        let fill = self.fill.as_ref().map(|(_, fill)| *fill);

        for &node in self
            .fields
            .iter()
            .chain(self.fill.as_ref().map(|(_, fill)| fill))
        {
            Cst::dispatch::<LowerCstToHir>(node, world, args);
        }

        args.commands.entity(entity).insert((
            Hir::TypedRecordConstructor,
            TypedRecordConstructor { ty, fields, fill },
        ));
    }
}

impl Operate<LowerCstToHir> for Type {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        _: Entity,
        _: &World,
        _: &mut LowerArgs<'x>,
    ) {
        unreachable!("Type should not be reached during CST lowering");
    }
}

impl Operate<LowerCstToHir> for NameAccess {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let function = args
            .walk_for(&QualifiedName {
                namespace: String::new(),
                name: self.name.1.clone(),
            })
            .unwrap_or_else(|| panic!("failed to find function definition {}", self.name.1));

        let function_args = self
            .args
            .as_ref()
            .map(|args| args.1.iter().copied().collect())
            .unwrap_or_default();

        args.commands.entity(entity).insert((
            Hir::FunctionCall,
            FunctionCall {
                function,
                args: function_args,
            },
        ));

        self.visit(|child| Cst::dispatch::<LowerCstToHir>(child, world, args));
    }
}

impl Operate<LowerCstToHir> for MemberAccess {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let function = args
            .walk_for(&QualifiedName {
                namespace: String::new(),
                name: self.name.1.clone(),
            })
            .unwrap_or_else(|| panic!("failed to find function definition {}", self.name.1));

        let function_args: Vec<Entity> = std::iter::once(&self.inner)
            .chain(self.args.iter().flat_map(|args| args.1.iter()))
            .copied()
            .collect();

        args.commands.entity(entity).insert((
            Hir::FunctionCall,
            FunctionCall {
                function,
                args: function_args,
            },
        ));

        self.visit(|child| Cst::dispatch::<LowerCstToHir>(child, world, args));
    }
}

impl Operate<LowerCstToHir> for ExternalFunc {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        let attributes = self.attributes.clone();
        let name = self.name.1.clone();

        let func_args = self
            .args
            .iter()
            .map(|ty| {
                let ty = world.get::<Type>(*ty).unwrap();
                args.walk_for(&QualifiedName::short(&ty.name.1)).unwrap()
            })
            .collect();

        for &node in self.attributes.iter() {
            Cst::dispatch::<LowerCstToHir>(node, world, args);
        }

        args.commands.entity(entity).insert((
            Hir::TypedExternalFunc,
            TypedExternalFunc {
                attributes,
                args: func_args,
                name,
            },
        ));
    }
}

#[impl_for_all(FieldInitializer, StringLiteral, CharLiteral, AttributeList, Invalid)]
impl Operate<LowerCstToHir> for This {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &self,
        entity: Entity,
        world: &World,
        args: &mut LowerArgs<'x>,
    ) {
        args.commands.entity(entity).insert(Hir::from(self));
        self.visit(|child| Cst::dispatch::<LowerCstToHir>(child, world, args));
    }
}
