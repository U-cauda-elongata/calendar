customElements.define('calendar-event', class extends HTMLElement {
	constructor() {
		super();
		this.attachShadow({ mode: 'open' });
	}

	connectedCallback() { this.update(); }

	attributeChangedCallback() { this.update(); }

	static get observedAttributes() {
		return ['data-title', 'data-timestamp', 'data-href', 'data-thumbnail'];
	}

	update() {
		const header = document.createElement('header');

		let grid;
		const href = this.dataset.href;
		if (href) {
			grid = document.createElement('a');
			grid.href = href;
			header.appendChild(grid);
		} else {
			grid = header;
		}
		grid.class = 'grid';

		const h = document.createElement('h2');
		h.textContent = this.dataset.title;
		grid.appendChild(h);

		const thumb = this.dataset.thumbnail;
		if (thumb) {
			const img = document.createElement('img');
			img.src = thumb;
			grid.appendChild(img);
		}

		const root = this.shadowRoot;
		root.replaceChildren(header);
		const template = document.getElementById('calendar-event-template');
		root.appendChild(template.content.cloneNode(true));
	}
});

// Displays a local date in a locale-specific way.
customElements.define('intl-date', class extends HTMLElement {
	constructor() {
		super();
		this.attachShadow({ mode: 'open' });
		this.shadowRoot.appendChild(document.createElement('time'));
	}

	connectedCallback() { this.update(); }

	attributeChangedCallback() { this.update(); }

	static get observedAttributes() { return ['lang','data-year','data-month', 'data-day']; }

	update() {
		// `attributeChangedCallback` is called every time Elm adds each data attribute, i.e.
		// `data-year`, `data-month` and `data-day`, even if the element is not connected to the
		// DOM, which would cause the `month` and `day` variables below to be `undefined`.
		if (!this.isConnected) {
			return;
		}

		const year = this.dataset.year;
		const month = this.dataset.month;
		const day = this.dataset.day;
		const date = new Date(year, month, day);

		const lang = this.getAttribute('lang');
		const opts = {
			month: 'long',
			day: 'numeric',
			weekday: 'short',
		};
		if (new Date().getYear() != date.getYear()) {
			opts.year = 'numeric';
		}
		const fmt = new Intl.DateTimeFormat(lang ?? 'default', opts);

		const time = this.shadowRoot.firstChild;
		if (lang) {
			time.removeAttribute('lang');
		} else {
			time.lang = fmt.resolvedOptions().locale;
		}
		time.dateTime = year.padStart(4, '0')
			+ '-' + String(+month + 1).padStart(2, '0')
			+ '-' + day.padStart(2, '0');

		time.textContent = fmt.format(date);
	}
});

// Displays a ECMAScript timestamp in a locale-specific way, converting it to local datetime.
customElements.define('intl-time', class extends HTMLElement {
	constructor() {
		super();
		this.attachShadow({ mode: 'open' });
		this.shadowRoot.appendChild(document.createElement('time'));
	}

	connectedCallback() { this.update(); }

	attributeChangedCallback() { this.update(); }

	static get observedAttributes() { return ['lang', 'data-timestamp']; }

	update() {
		const date = new Date(+this.dataset.timestamp);

		const lang = this.getAttribute('lang');
		const fmt = new Intl.DateTimeFormat(lang ?? 'default', {
			hour: 'numeric',
			minute: 'numeric',
		});

		const time = this.shadowRoot.firstChild;
		if (lang) {
			time.removeAttribute('lang');
		} else {
			time.lang = fmt.resolvedOptions().locale;
		}

		time.dateTime = date.toISOString();
		time.textContent = fmt.format(date);
	}
});
