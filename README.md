# Lambda

A simple lambda-calculus parser, type-inferer and interpreter written in Haskell.

## Language

```
⟨e⟩ → λ.⟨e⟩
   | ⟨e⟩ ⟨e⟩
   | ⟨x⟩
   | let ⟨x⟩ = ⟨e⟩ in ⟨e⟩
   | case ⟨e⟩ of {⟨p⟩}
   | if ⟨e⟩ then ⟨e⟩ else ⟨e⟩
   | (⟨e⟩, ⟨e⟩)
   | (⟨e⟩)
   | <c>
```

## Usage

## License
