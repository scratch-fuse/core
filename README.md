# FUSE: Language that compiles to Scratch

FUSE is a low-level programming language that compiles to Scratch. It is designed to be similar to Rust and TypeScript, making it easier for developers familiar with those languages to learn and use. FUSE aims to provide a more structured and powerful way to create Scratch projects, while still being accessible to beginners.

```fuse
fn main() once -> void {
  looks.say("Hello, World!")
}

event.start {
  main()
}
```

## Features

- **Familiar syntax** for developers with experience in Rust and TypeScript
- **Compiles to Scratch**, allowing for easy sharing and execution of projects
- **Supports variables, lists, and custom blocks**
- **Event-driven programming** with Scratch hat blocks
- **Type annotations** for better code clarity
- **Decorators** for external block integration
- Can be used as **Intermediate Representation (IR)** for other languages, or as a component for Scratch Addons

## Examples

### Basic Hello World

```fuse
fn main() once -> void {
  looks.say("Hello, World!")
}

event.start {
  main()
}
```

### Variables and Conditionals

```fuse
global score = 0
global playerName = "Player"

fn checkScore() once -> void {
  if (score >= 100) {
    looks.say(playerName .. " wins!")
  } else {
    looks.say("Keep trying!")
  }
}

event.start {
  checkScore()
}
```

### Loops and Arrays

```fuse
global numbers = [1, 2, 3, 4, 5]
global sum = 0
global i = 0

fn calculateSum() once -> void {
  for (i = 0; i < numbers.length; i++) {
    sum += numbers[i]
  }
  looks.say("Sum: " .. sum)
}

event.start {
  calculateSum()
}
```

### Custom Functions with Parameters

```fuse
fn greet(name: any, times: any) once -> void {
  let counter = 1
  while (counter <= times) {
    looks.say("Hello, " .. name .. "!")
    counter++
  }
}

event.start {
  greet("Scratch", 3)
}
```

### Event Handling

```fuse
global speed = 10

fn moveCharacter() once -> void {
  motion.moveSteps(speed)
  if (sensing.touchingObject("_edge_")) {
    motion.ifOnEdgeBounce()
  }
}

event.start {
  loop {
    moveCharacter()
  }
}

event.keyPressed("space") {
  speed += 5
}
```

### Using External Blocks with Decorators

```fuse
// Define external Scratch blocks using @extern decorator
@extern("wait 1 frame") fn yield() once -> void {
  // External block implementation
}

@extern("current timestamp") fn currentTimestamp() once -> any {
  return sensing.daysSince2000() * 86400 * 1000
}

fn main() once -> void {
  let startTime = currentTimestamp()
  yield()
  let endTime = currentTimestamp()
  looks.say("Frame time: " .. (endTime - startTime) .. "ms")
}

event.start {
  main()
}
```

### FPS Counter Example

```fuse
@extern("last timestamp") let lastTimestamp = 0

@extern("current timestamp") fn currentTimestamp() once -> any {
  return sensing.daysSince2000() * 86400 * 1000
}

@extern("wait 1 frame") fn yield() once -> void {}

fn calculateFps() once -> any {
  lastTimestamp = currentTimestamp()
  yield()
  return 1000 / (currentTimestamp() - lastTimestamp)
}

global fps = 0

fn main() once -> void {
  loop {
    fps = calculateFps()
    looks.say("FPS: " .. fps)
  }
}

event.start {
  main()
}
```

## Use Cases

- **Badger**: FUSE works as secondary IR for Badger, a language with complex features (temporary variables, OOP, etc.).
- **FSE (FUSE-based Script Editing, aka Fluent Script Editing)**: FUSE works as the language for FSE, a Scratch Addon that provides users a single-line input box to insert scripts immediately with autocomplete, syntax highlighting, and even AI assistance.
- **Educational Tools**: Create structured programming lessons for Scratch users transitioning to text-based languages.
- **Code Generation**: Use FUSE as a target language for visual programming tools or domain-specific languages.

## Documentation

For detailed language specification, see [specification.md](specification.md).

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
