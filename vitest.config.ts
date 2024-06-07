import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    include: ['**/__tests__/**/*.spec.[tj]s'],
    exclude: [
      '**/node_modules/**',
      './playground/**/*.*',
    ],
  },
  publicDir: false,
})
