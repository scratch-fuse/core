const { Lexer, Parser, toSource } = require('../dist/index.js');

function format(input) {
  const tokens = new Lexer(input).all();
  const ast = new Parser(tokens).parse();
  const source = toSource(ast, 2);
  return source;
}

if (require.main === module) {
  const fs = require('fs');
  const path = require('path');
  
  const inputPath = process.argv[2];
  if (!inputPath) {
    console.error('Usage: node formatter.cjs <input-file>');
    process.exit(1);
  }
  
  const input = fs.readFileSync(path.resolve(inputPath), 'utf-8');
  const output = format(input);
  console.log(output);
}

module.exports = { format };