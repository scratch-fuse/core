import { Lexer, Token, TokenType } from './lexer';
import { Parser, ParserError } from './parser';
import {
  Deserializer,
  Scope,
  ScratchFunction
} from './deserializer/deserializer';
import { Script, Variable } from './deserializer/base';

import fs from 'fs/promises';

(async () => {
  console.log('Testing Scratch FUSE deserializer...');
  console.log('=====================================');

  try {
    // Read the test file
    const parserTestCode = await fs.readFile(
      '/home/furryr/scratch-fuse/test.fuse',
      'utf-8'
    );

    // Create a Lexer instance
    const lexer = new Lexer(parserTestCode);

    // Create a Parser instance
    const parser = new Parser(lexer.all());

    // Parse the tokens into an AST
    const ast = parser.parse();
    console.log('✓ Parsing completed successfully');

    // Extract variables and functions from AST
    const variables = new Map<string, Variable>();
    const functions: ScratchFunction[] = [];
    const namespaces = new Map();

    // Process declarations to extract variables and functions
    for (const decl of ast.body) {
      if (decl.type === 'VariableDeclaration') {
        const varDecl = decl as any; // Type assertion for demo
        const variable: Variable = {
          name: varDecl.identifier,
          type: 'scalar', // Default to scalar, will be updated if array is detected
          isGlobal: varDecl.isGlobal,
          value: undefined
        };

        // Check if initializer is an array
        if (
          varDecl.initializer &&
          varDecl.initializer.type === 'ArrayExpression'
        ) {
          variable.type = 'list';
        }

        variables.set(varDecl.identifier, variable);
        console.log(
          `✓ Found variable: ${varDecl.identifier} (${variable.type}, ${
            variable.isGlobal ? 'global' : 'local'
          })`
        );
      } else if (decl.type === 'FunctionDeclaration') {
        const funcDecl = decl as any; // Type assertion for demo
        const func = new ScratchFunction(variables, funcDecl);
        functions.push(func);
        console.log(
          `✓ Found function: ${funcDecl.name} with ${funcDecl.parameters.length} parameters`
        );
      } else if (decl.type === 'NamespaceDeclaration') {
        const nsDecl = decl as any; // Type assertion for demo
        const namespaceMap = new Map();

        // Process namespace properties
        if (nsDecl.body && nsDecl.body.properties) {
          for (const prop of nsDecl.body.properties) {
            if (prop.value && typeof prop.value === 'object') {
              const entry = {
                opcode: prop.value.opcode || '',
                type: prop.value.type || 'command',
                args: prop.value.args || []
              };
              namespaceMap.set(prop.key, entry);
            }
          }
        }

        namespaces.set(nsDecl.name, namespaceMap);
        console.log(`✓ Found namespace: ${nsDecl.name}`);
      }
    }

    // Create deserializer
    const deserializer = new Deserializer(variables, functions, namespaces);

    // Deserialize to Scratch scripts
    const scripts = deserializer.parse(ast);

    console.log('\n✓ Deserialization completed successfully');
    console.log(`✓ Generated ${scripts.length} script(s)`);

    // Output the results
    console.log('\n--- Generated Scratch Scripts ---');
    scripts.forEach((script, index) => {
      console.log(`\nScript ${index + 1}:`);
      console.log(JSON.stringify(script, null, 2));
      // console.log(`\nScript ${index + 1}:`);
      // if (script.hat) {
      //   console.log(`  Hat block: ${script.hat.opcode}`);
      // }
      // console.log(`  Blocks: ${script.blocks.length} block(s)`);

      // // Show first few blocks for preview
      // script.blocks.slice(0, 3).forEach((block, blockIndex) => {
      //   console.log(`    ${blockIndex + 1}. ${block.opcode}`);
      // });

      // if (script.blocks.length > 3) {
      //   console.log(`    ... and ${script.blocks.length - 3} more block(s)`);
      // }
    });

    // console.log('\n--- Variables ---');
    // variables.forEach((variable, name) => {
    //   console.log(
    //     `  ${name}: ${variable.type} (${
    //       variable.isGlobal ? 'global' : 'local'
    //     })`
    //   );
    // });

    // console.log('\n--- Functions ---');
    // functions.forEach(func => {
    //   console.log(
    //     `  ${func.decl.name}: ${func.decl.parameters.length} parameter(s)`
    //   );
    //   console.log(`    Proccode: ${func.proccode}`);
    // });
  } catch (error) {
    console.error('❌ Error during testing:', error);
    if (error instanceof ParserError) {
      console.error('Parser errors:', error.errors);
    }
  }
})();
