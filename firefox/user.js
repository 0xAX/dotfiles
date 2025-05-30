// userchrome.css usercontent.css activate
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Fill SVG Color
user_pref("svg.context-properties.content.enabled", true);

// CSS Blur Filter - 88 Above
user_pref("layout.css.backdrop-filter.enabled", true);

// Restore Compact Mode - 89 Above
user_pref("browser.compactmode.show", true);

// Enable hardware acceleration for rendering layers
user_pref("layers.acceleration.force-enabled", true);

// Enable WebRender on all possible GPUs
user_pref("gfx.webrender.all", true);

// Enable WebRender
user_pref("gfx.webrender.enabled", true);

// Enables CSS `backdrop-filter` support.
user_pref("layout.css.backdrop-filter.enabled", true);

// Allows content to use the `context-properties` attribute in SVG,
// enabling advanced styling via CSS variables.
user_pref("svg.context-properties.content.enabled", true);

// Disables the redesigned (revamped) Firefox sidebar UI.
// Setting this to false keeps the classic sidebar.
user_pref("sidebar.revamp", false);
