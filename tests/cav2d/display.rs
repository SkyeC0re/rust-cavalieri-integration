use cavint::core::{parsing::DefaultContext, differentiable::AD};


#[test]
fn default_context_ad() {
    let x = DefaultContext::<AD>::default();
}