import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, ".", "AIBO_");
  return {
    plugins: [react()],
    server: {
      port: 5173,
      strictPort: true,
      proxy: {
        "/api": {
          target: env.AIBO_WEB_API_URL || "http://127.0.0.1:5000",
          changeOrigin: true,
          ws: true,
        },
      },
    },
    build: {
      outDir: "../python/src/aibo/resources/web",
      emptyOutDir: true,
    },
  };
});
