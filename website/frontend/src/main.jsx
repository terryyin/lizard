import { createRoot } from "react-dom/client";
import App from "./App.jsx";
import "./site.css";

const config = JSON.parse(document.getElementById("lizard-config").textContent);
createRoot(document.getElementById("root")).render(<App config={config} />);
