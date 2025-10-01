export class ErrorList extends Error {
  constructor(public errors: Error[]) {
    const message = errors.map(err => err.stack).join('\n  ')
    super('Multiple errors:\n  ' + message)
    this.name = 'ErrorList'
  }
}
