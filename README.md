# FUSE: Language that compiles to Scratch

FUSE is a low-level programming language that compiles to Scratch. It is designed to be similar to Rust and Typescript language, making it easier for developers familiar with those languages to learn and use. FUSE aims to provide a more structured and powerful way to create Scratch projects, while still being accessible to beginners.

```groovy
fn main() once -> void {
  looks.say("Hello, World!")
}

event.start {
  main()
}
```

## Features

- Familiar syntax for developers with experience in Rust and Typescript
- Compiles to Scratch, allowing for easy sharing and execution of projects
- Supports variables, lists, and custom blocks
- Provides a more structured approach to programming in Scratch
- Can be used as Intermediate Representation (IR) for other languages, or as a component for Scratch Addons

## Use cases

- Badger: FUSE works as secondary IR for Badger, a language with complex features (temporary variables, OOP, etc.).
- FSE (FUSE-based Script Editing, aka Fluent Script Editing): FUSE works as the language for FSE, a Scratch Addon that provides user an single-line input box to insert scripts immediately with autocomplete, syntax highlighting, and even AI assistance.

## Todos

- [x] Implement basic syntax and semantics
- [x] Implement compiler to convert FUSE code to Scratch blocks
- [x] Create documentation and examples
- [ ] Decompiler from Scratch blocks to FUSE code
- [ ] Develop LSP with syntax highlighting and autocomplete

## Special Thanks

- This project is inspired by SteveXMH's [scratch-script](https://github.com/Steve-xmh/scratch-script), which provides a similar functionality for Scratch. This project is a successor to it, with a focus on a more fancy syntax and better structure.

## License

This project is licensed under the MPL-2.0 License. See the [LICENSE](LICENSE) file for details.
