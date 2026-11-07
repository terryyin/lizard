export default function Arrow({ children = "↗" }) {
  return <span aria-hidden="true">{children}</span>;
}
