import tsup from 'tsup'

export default tsup.defineConfig({
  entry: ['src/index.ts'],
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: false,
  clean: true,
  minify: false,
  target: 'esnext',
  outDir: 'dist',
  loader: { '.fuse': 'text' }
})
