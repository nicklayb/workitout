import * as esbuild from 'esbuild';
import ElmPlugin from 'esbuild-plugin-elm';
import { readFileSync, writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';
import postcss from 'postcss';
import tailwindcss from '@tailwindcss/postcss';

const isDev = process.argv.includes('--watch');
const distDir = 'dist';

// Plugin pour compiler Tailwind CSS 4 avec PostCSS
const tailwindPlugin = {
  name: 'tailwind-css-v4',
  setup(build) {
    build.onLoad({ filter: /app\.css$/ }, async (args) => {
      try {
        const css = readFileSync(args.path, 'utf8');

        // Traiter avec PostCSS et Tailwind v4
        const result = await postcss([tailwindcss()]).process(css, {
          from: args.path,
          to: join(distDir, 'styles.css')
        });

        return {
          contents: result.css,
          loader: 'css'
        };
      } catch (error) {
        console.error('Tailwind CSS error:', error);
        return {
          errors: [{
            text: error.message,
            location: { file: args.path }
          }]
        };
      }
    });
  }
};

// Configuration esbuild
const buildConfig = {
  entryPoints: ['src/index.js'],
  bundle: true,
  outdir: distDir,
  plugins: [
    ElmPlugin({
      debug: isDev,
      optimize: !isDev,
      pathToElm: 'elm'
    }),
    tailwindPlugin
  ],
  loader: {
    '.html': 'copy',
    '.png': 'file',
    '.jpg': 'file',
    '.svg': 'file'
  },
  minify: !isDev,
  sourcemap: isDev,
  logLevel: 'info'
};

// Créer le dossier dist
mkdirSync(distDir, { recursive: true });

// Build ou watch
if (isDev) {
  const ctx = await esbuild.context(buildConfig);
  await ctx.serve({
    servedir: './',
  });
  console.log('👀 Watching for changes...');
} else {
  await esbuild.build(buildConfig);
  console.log('✅ Build complete!');
}
