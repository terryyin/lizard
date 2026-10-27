import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// Flask (index.py) serves this output at /website/static/dist/ and reads its manifest.
export default defineConfig({
  plugins: [react()],
  base: "/website/static/dist/",
  build: {
    outDir: "../static/dist",
    emptyOutDir: true,
    manifest: "manifest.json",
    rolldownOptions: { input: "src/main.jsx" },
  },
});
