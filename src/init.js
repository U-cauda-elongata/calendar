const feeds = [
	{
		"id": "yt:channel:UCEOugXOAfa-HRmRjKbH8z3Q",
		"title": "けものフレンズプロジェクト公式",
		"icon": "https://yt3.ggpht.com/ryEtopDlUPjueM1j3UufZ3UrGCpuYxc5tdeX5-pTlkjygXqbw7j29bFIPu8uCy4NzHAM1EetmLM=s60",
	},
	{
		"id": "yt:channel:UCmYO-WfY7Tasry4D1YB4LJw",
		"title": "フンボルトペンギン / Humboldt Penguin",
		"icon": "https://yt3.ggpht.com/ytc/AKedOLSr75ivVQI4bHcaoMOaYxPjbRnL3-2VCSNuHbJ-=s60",
	},
	{
		"id": "yt:channel:UCMpw36mXEu3SLsqdrJxUKNA",
		"title": "シマハイイロギツネ / Island Fox",
		"icon": "https://yt3.ggpht.com/2ohbFqFqLbEw66rWMhTjb-wpa5X9APonb1KZiiBJbmGcS69yKUwtmLSHfhPUSF4snFp1O9r_=s60",
	},
	{
		"id": "yt:channel:UCabMjG8p6G5xLkPJgEoTnDg",
		"title": "コヨーテ / Coyote",
		"icon": "https://yt3.ggpht.com/VwLc2w11lh_JlrsDB9P4OvBJpqaoAZdS08gqQx7vtJ5-4DjsEiP5Un6xmT0q8VE6zr8uXYEnTqg=s60",
	},
	{
		"id": "yt:channel:UCdNBhcAohYjXlUVYsz8X2KQ",
		"title": "ダイアウルフ / Dire Wolf",
		"icon": "https://yt3.ggpht.com/yJUytRAl-MwBrGmIXMdctRgcNuAborghz1SGt2o6KDewrB1aP6saZLdX9HWBPdF5JEutZ6aBKNY=s60",
	},
	{
		"id": "yt:channel:UCxm7yNjJsSvyvcG96-Cvmpw",
		"title": "カラカル / Caracal",
		"icon": "https://yt3.ggpht.com/PfYRtTGfYRkpluFsqAjYgSERbvu7wQ-pNpRARUeSTvka1pa-t3rFgb4DhxNuhDikmz-BP8WLgw=s60",
	},
	{
		"id": "yt:channel:UCEcMIuGR8WO2TwL9XIpjKtw",
		"title": "ケープペンギン / African Penguin",
		"icon": "https://yt3.ggpht.com/ytc/AKedOLSiSzCCyj5TBipvNgNcz0NrPZbvJZmZQU9JUFE-=s60",
	},
];

const app = Elm.Main.init({
	flags: {
		features: {
			copy: !!navigator.clipboard,
			share: !!navigator.share,
		},
		languages: navigator.languages,
		feeds,
	},
});

app.ports.setLang.subscribe(lang => {
	const h = document.documentElement;
	h.lang = lang;
	h.classList.add('lang-confirmed');
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
					app.ports.onScrollToBottom.send(null);
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
