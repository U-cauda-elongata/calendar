const app = Elm.Main.init({
	flags: {
		features: {
			copy: !!navigator.clipboard,
			share: !!navigator.share,
		},
		languages: navigator.languages,
		feeds,
		observances,
	},
});

app.ports.setLang.subscribe(lang => {
	const h = document.documentElement;
	h.lang = lang;
	h.classList.add('lang-confirmed');
});

app.ports.preventScrollFocus.subscribe(id => {
	const node = document.getElementById(id);
	if (node) {
		node.focus({ preventScroll: true });
	}
});

app.ports.slideViewportInto.subscribe(id => {
	const node = document.getElementById(id);
	if (node) {
		requestAnimationFrame(() => {
			node.scrollIntoView({ behavior: 'smooth', block: 'center' });
		});
	}
});

app.ports.showModal.subscribe(id => {
	const node = document.getElementById(id);
	if (node) {
		if (node.showModal) {
			node.showModal();
		} else {
			node.toggleAttribute('open', true);
		}
	}
});

app.ports.close.subscribe(id => {
	const node = document.getElementById(id);
	if (node) {
		if (node.close) {
			node.close();
		} else {
			node.toggleAttribute('open', false);
		}
	}
});

app.ports.copy.subscribe(text => {
	navigator.clipboard.writeText(text);
});

app.ports.share.subscribe(data => {
	navigator.share(data).catch(() => {})
});

let ticking = false;
let feedBottom;
function listener() {
	if (!ticking) {
		requestAnimationFrame(() => {
			feedBottom = feedBottom || document.getElementById('feedBottom');
			if (feedBottom) {
				const viewportBottom = scrollY + document.documentElement.clientHeight;
				if (viewportBottom > feedBottom.offsetTop) {
					app.ports.onScrollToBottom.send();
				}
			}
			ticking = false;
		});
		ticking = true;
	}
}
addEventListener('scroll', listener);

app.ports.removeScrollEventListener.subscribe(() => {
	removeEventListener('scroll', listener);
});
