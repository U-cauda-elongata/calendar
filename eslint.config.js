import globals from "globals";
import pluginJs from "@eslint/js";
import tseslint from "typescript-eslint";


export default [
  {
    files: ["**/*.{js,mjs,cjs,ts}"],
    rules: {
      "@typescript-eslint/switch-exhaustiveness-check": "error",
    },
  },
  {
    languageOptions: {
      globals: globals.browser,
      parserOptions: {
		project: true,
	  }
    }
  },
  pluginJs.configs.recommended,
  ...tseslint.configs.recommended,
];
