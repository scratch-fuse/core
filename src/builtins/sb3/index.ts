import { getProgramInfo } from '../../compiler'
import { Lexer } from '../../lexer'
import { Parser } from '../../parser'
import fuse from './builtin.fuse'

export const Sb3NamespacesRaw = fuse

export const Sb3Namespaces = (() => {
  const lexer = new Lexer(Sb3NamespacesRaw)
  const parser = new Parser(lexer.all())
  const program = parser.parse()
  return getProgramInfo(program).namespaces
})()
