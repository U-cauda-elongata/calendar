import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';
import { createHtmlPlugin } from 'vite-plugin-html'

export default defineConfig({
	build: {
		// GitHub Pages somehow only allows you to set `/` or `/docs` as the root directory.
		outDir: 'docs',
	},
	plugins: [
		elmPlugin(),
		createHtmlPlugin({
			minify: true,
		}),
	],
});
