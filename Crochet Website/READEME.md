# Handmade Crochet Shop Website

A single-page, self-contained e-commerce website built for a real handmade crochet business, designed and coded from scratch (HTML/CSS/JavaScript, no framework or build step required).

## Overview

The site serves as a small business's full storefront and customer-relationship hub: product browsing, custom order intake, live customer reviews, and order cancellation — all in one static HTML file that can be hosted anywhere (GitHub Pages, Netlify, or a basic web host) with no backend server required.

## Features

- **Responsive landing page** with hero section, brand identity (custom color palette, Fraunces/Quicksand typography), and a horizontally-scrolling photo gallery of finished pieces
- **Product shop section** with per-item configuration (type, colour, quantity) and live price calculation including delivery costs
- **Custom order form** — customers submit size, colour preferences, deadline, delivery address, and an optional reference photo
- **Dual-path order handling**: orders submit via [Formspree](https://formspree.io/) (serverless form backend) with an automatic `mailto:` fallback if the network request fails, so no order is ever lost
- **Automated customer confirmation emails** sent on successful order submission
- **Live review system** built on [Firebase Firestore](https://firebase.google.com/) — reviews post instantly to the page (read/create only, no update/delete, enforced via Firestore security rules) with a star-rating picker and graceful fallback if Firebase isn't configured
- **Order cancellation form**, submitted through a separate Formspree endpoint
- **No backend server or database required** — Formspree and Firebase handle all persistence and email delivery

## Tech Stack

- Vanilla HTML5, CSS3 (custom properties/variables for theming), and JavaScript (ES6+, async/await)
- [Formspree](https://formspree.io/) — form-to-email delivery for orders and cancellations
- [Firebase Firestore](https://firebase.google.com/docs/firestore) — live review storage and retrieval
- Google Fonts (Fraunces, Quicksand)

## Setup

1. Clone or download `index.html`
2. Update the `FORM_ENDPOINT` and `CANCEL_FORM_ENDPOINT` constants with your own Formspree form IDs
3. Update the `FIREBASE_CONFIG` object with your own Firebase project credentials
4. In the Firebase console, set Firestore security rules to allow public read/create but not update/delete on the `reviews` collection:
   ```
   match /reviews/{reviewId} {
     allow read: if true;
     allow create: if true;
     allow update, delete: if false;
   }
   ```
5. Host the file anywhere that serves static HTML (GitHub Pages, Netlify, Vercel, or a basic web server)

## Live site

[(https://hookedonyarnbk.netlify.app/)]

**Tools:** HTML5, CSS3, JavaScript, Formspree, Firebase Firestore
