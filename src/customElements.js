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
	}

	connectedCallback() {
		if (!this.firstChild) {
			this.appendChild(document.createElement('time'));
		}
		this.update();
	}

	attributeChangedCallback() { this.update(); }

	static get observedAttributes() { return ['lang', 'data-timestamp']; }

	update() {
		if (!this.isConnected) {
			return;
		}

		const date = new Date(+this.dataset.timestamp);

		const lang = this.getAttribute('lang');
		const fmt = new Intl.DateTimeFormat(lang ?? 'default', {
			hour: 'numeric',
			minute: 'numeric',
		});

		const time = this.firstChild;
		if (lang) {
			time.removeAttribute('lang');
		} else {
			time.lang = fmt.resolvedOptions().locale;
		}

		time.dateTime = date.toISOString();

		// Set the inner HTML to something like `12<span class="time-separator">:</span>00 AM`.
		// Reuse `<span>` elements from a previous `update()` call if possible, in order not to
		// reset CSS animation of the element, which complicates the implementation a lot (meh).
		const parts = fmt.format(date).split(':');
		const nodes = Array.from(time.childNodes);
		const first = parts.shift();
		let node = nodes.shift();
		if (first) {
			if (node) {
				time.insertBefore(document.createTextNode(first), node);
			} else {
				time.append(first);
			}
			for (const part of parts) {
				while (node) {
					if (node.tagName == 'SPAN') {
						break;
					}
					time.removeChild(node);
					node = nodes.shift();
				}
				if (node) {
					node.insertAdjacentText('afterend', part);
					node = nodes.shift();
				} else {
					const separator = document.createElement('span');
					separator.className = 'time-separator';
					separator.textContent = ':';
					time.appendChild(separator);
					time.appendChild(document.createTextNode(part));
				}
			}
		}
		while (time.contains(node)) {
			time.removeChild(node);
		}
	}
});