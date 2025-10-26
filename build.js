import * as esbuild from 'esbuild';
import http from "http";
import ElmPlugin from 'esbuild-plugin-elm';
import { readFileSync, existsSync, mkdirSync } from 'fs';
import { fileURLToPath } from "url";
import path from 'path';
import postcss from 'postcss';
import tailwindcss from '@tailwindcss/postcss';

const isDev = process.argv.includes('--watch');
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const distDir = 'dist';

const tailwindPlugin = {
  name: 'tailwind-css-v4',
  setup(build) {
    build.onLoad({ filter: /app\.css$/ }, async (args) => {
      try {
        const css = readFileSync(args.path, 'utf8');

        const result = await postcss([tailwindcss()]).process(css, {
          from: args.path,
          to: path.join(distDir, 'styles.css')
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

mkdirSync(distDir, { recursive: true });


function startServer() {
  return http
    .createServer((req, res) => {
      const indexFile = path.join(__dirname, "index.html")
      const filePath = path.join(__dirname, req.url === "/" ? "/index.html" : req.url);

      if (existsSync(filePath)) {
        res.writeHead(200);
        res.end(readFileSync(filePath));
      } else {
        const index = readFileSync(indexFile);
        res.writeHead(200, { "Content-Type": "text/html" });
        res.end(index);
      }
    })
    .listen(3000, () => {
      console.log("🚀 Serveur dispo sur http://localhost:3000");
    });
}

if (isDev) {
  const ctx = await esbuild.context(buildConfig);
  await ctx.watch();
  startServer()
  console.log('👀 Watching for changes...');
} else {
  await esbuild.build(buildConfig);
  console.log('✅ Build complete!');
}


