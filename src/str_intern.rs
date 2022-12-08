use std::{collections::HashMap, sync::RwLock};

use once_cell::sync::Lazy;

#[derive(Default)]
struct Interner {
    cache: RwLock<HashMap<&'static str, &'static str>>,
}

static INTERNER: Lazy<Interner> = Lazy::new(Default::default);

pub(crate) fn intern(s: &str) -> &'static str {
    if let Some(s) = INTERNER.cache.read().unwrap().get(s) {
        return s;
    }
    let static_s = Box::leak(s.to_string().into_boxed_str());
    INTERNER.cache.write().unwrap().insert(static_s, static_s);
    static_s
}
