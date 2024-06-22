# Lambda

A simple lambda-calculus parser, type-inferer and interpreter written in Haskell.

## Language

```
⟨e⟩ → λ.⟨e⟩ | ⟨e⟩ ⟨e⟩ | ⟨x⟩ | <c>
```

```
⟨τ⟩ → ⟨α⟩ | ⟨ι⟩ | ⟨τ⟩ → ⟨τ⟩
```

```
⟨σ⟩ → ⟨τ⟩ | ∀⟨α⟩.⟨σ⟩
```

## Usage

## License
