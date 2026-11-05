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

const DIST_FOLDER = path.join(__dirname, "dist")

// Use the compiler pinned in package.json rather than whatever `elm` is on PATH.
const ELM_COMPILER_PATH = path.join(__dirname, "node_modules", ".bin", "elm")

const distFolder = url => path.join(DIST_FOLDER, url)

const VERSION_PATH = path.join(__dirname, "VERSION")
const SEMANTIC_VERSION_PATTERN = /^\d+\.\d+\.\d+$/

// Reads the application version from the VERSION file, failing the build
// when it isn't a semantic version (MAJOR.MINOR.PATCH).
function readVersion() {
  const version = readFileSync(VERSION_PATH, "utf8").trim()
  if (!SEMANTIC_VERSION_PATTERN.test(version)) {
    throw new Error(`VERSION must be MAJOR.MINOR.PATCH, got "${version}"`)
  }
  return version
}

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
  define: {
    APP_VERSION: JSON.stringify(readVersion())
  },
  outdir: distDir,
  plugins: [
    ElmPlugin({
      debug: isDev,
      optimize: !isDev,
      pathToElm: ELM_COMPILER_PATH
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
      let filePath = distFolder(req.url === "/" ? "/index.html" : req.url);

      if (!existsSync(filePath)) {
        filePath = distFolder("index.html")
        res.setHeader("Content-Type", "text/html")
      }

      console.info(`${req.method} ${req.url} -> ${filePath.replace(DIST_FOLDER, "")}`)

      res.writeHead(200);
      res.end(readFileSync(filePath));
    })
    .listen(3000, () => {
      console.log("🚀 Server running at http://localhost:3000");
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
