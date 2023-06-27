use itertools::Itertools;
use crate::inet::{INet, new_inet, ROOT};
use super::{Term, alloc_at, to_net};
use std::{collections::HashMap, vec};

pub type DefinitionName = String;
pub type DefinitionId = u32;

// TODO: Are all these fields necessary?
#[derive(Clone)]
pub struct DefinitionData {
  pub name: DefinitionName,
  pub term: Term,
  pub net: INet,
}

#[derive(Default, Clone)]
pub struct DefinitionBook {
  pub definition_name_to_id: HashMap<DefinitionName, DefinitionId>,
  pub definition_id_to_data: Vec<DefinitionData>,
}

impl DefinitionBook {
  pub fn new(definition_name_to_term: HashMap<String, Term>) -> Self {
    let (definition_name_to_id, definition_id_to_name_and_term): (HashMap<_, _>, Vec<_>) =
      definition_name_to_term
        .iter()
        .sorted_by_key(|&(name, _)| name)
        .enumerate()
        .map(|(id, (name, term))| ((name.clone(), id as DefinitionId), (name, term)))
        .unzip();

    let definition_id_to_data = definition_id_to_name_and_term
      .into_iter()
      .map(|(name, term)| DefinitionData {
        name: name.clone(),
        term: term.clone(),
        net: to_net(&term, &definition_name_to_id),
      })
      .collect_vec();

    Self {
      definition_name_to_id,
      definition_id_to_data,
    }
  }

  pub fn contains(&self, name: &str) -> bool {
    self.definition_name_to_id.keys().any(|n| n == name)
  }

  pub fn add_definition(&mut self, name: DefinitionName, term: Term) {
    debug_assert!(!self.contains(&name), "{}", name);
    let id = self.definition_id_to_data.len() as DefinitionId;
    self.definition_name_to_id.insert(name.clone(), id);
    self.definition_id_to_data.push(DefinitionData {
      net: to_net(&term, &self.definition_name_to_id),
      name,
      term,
    });
  }

  pub fn extract_closed_subterms(&mut self) {
    let mut extracted_definition_book = self.clone();
    let mut idx = 0;
    let mut ctx = vec![];
    let updated_definition_terms = self.definition_id_to_data.iter().map(|data| {
      let r = data.term.clone().extract_closed_subterms(self, &mut extracted_definition_book, &mut idx, &mut ctx);
      debug_assert_eq!(ctx, Vec::<Vec<_>>::new());
      r
    }).collect_vec();
    for (term, data) in updated_definition_terms.into_iter().zip(&mut extracted_definition_book.definition_id_to_data) {
      data.term = term;
    }
    *self = extracted_definition_book;
    // TODO: Afterwards, merge duplicate (extracted) definitions
  }

  pub fn print(&self) {
    println!("---");
    for DefinitionData { name, term, .. } in &self.definition_id_to_data {
      println!("{name}: {term}");
    }
    println!("---");
  }
}
