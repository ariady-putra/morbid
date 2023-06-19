/** @type {import('next').NextConfig} */
const nextConfig = {
  experimental: {
    outputStandalone: true,
  },
  reactStrictMode: true,
  webpack: (config) => {
    config.experiments = {
      asyncWebAssembly: true,
      topLevelAwait: true,
      layers: true,
    };
    return config;
  },
};

// do NOT convert this into an ES module, it will fail during `yarn dev`!
module.exports = nextConfig; // export default nextConfig;
// SyntaxError: Unexpected token 'export', error Command failed with exit code 1.
