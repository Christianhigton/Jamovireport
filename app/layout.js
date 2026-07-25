import './globals.css';

export const metadata = {
  title: 'jReport QA Checklist',
  description: 'Interactive QA checklist for jReport standalone analyses and jamovi add-ons.',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
