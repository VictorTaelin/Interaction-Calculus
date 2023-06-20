use itertools::Itertools;
use crate::inet::{INet, new_inet, ROOT};
use super::{Term, alloc_at};
use std::collections::HashMap;

pub type DefinitionName = String;
pub type DefinitionId = u32;

pub struct DefinitionData {
  pub name: DefinitionName,
  pub term: Term,
  pub net: INet,
}

pub struct DefinitionBook {
  pub definition_name_to_term: HashMap<DefinitionName, Term>,
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
      .iter()
      .map(|(name, term)| DefinitionData {
        name: (*name).clone(),
        term: (*term).clone(),
        net: {
          let mut net = new_inet();
          alloc_at(&mut net, term, ROOT, &definition_name_to_id);
          net
        },
      })
      .collect_vec();

    Self {
      definition_name_to_term,
      definition_name_to_id,
      definition_id_to_data,
    }
  }
}
