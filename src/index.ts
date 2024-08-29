import { Elm } from './Main.elm';
import './customElements';
import feeds from './feeds';
import observances from './observances';

const app = Elm.Main.init({
	flags: {
		features: {
			copy: !!navigator.clipboard,
			share: !!navigator.share,
		},
		// Elm won't mutate the array.
		// @ts-expect-error: TS4104 The type 'readonly string[]' is 'readonly' and cannot be assigned to the mutable type 'string[]'.
		languages: navigator.languages,
		feeds,
		observances,
	},
});

let ticking = false;
let feedBottom: HTMLElement | null;
function listener() {
    if (!ticking) {
        requestAnimationFrame(() => {
            feedBottom = feedBottom || document.getElementById('feedBottom');
            if (feedBottom) {
                const viewportBottom = scrollY + document.documentElement.clientHeight;
                if (viewportBottom > feedBottom.offsetTop) {
                    app.ports.interopToElm.send({
                        tag: 'OnScrollToBottom',
                    });
                }
            }
            ticking = false;
        });
        ticking = true;
    }
}
addEventListener('scroll', listener);

app.ports.interopFromElm.subscribe((fromElm) => {
	switch (fromElm.tag) {
		case 'close':
			{
				const node = document.getElementById(fromElm.data);
				if (node) {
					if ('close' in node) {
						(node as HTMLDialogElement).close();
					} else {
						node.toggleAttribute('open', false);
					}
				}
			}
			break;
		case 'copy':
			navigator.clipboard.writeText(fromElm.data);
			break;
		case 'preventScrollFocus':
			{
				const node = document.getElementById(fromElm.data);
				if (node) {
					node.focus({ preventScroll: true });
				}
			}
			break;
		case 'removeScrollEventListener':
			removeEventListener('scroll', listener);
			break;
		case 'setLang': {
			const h = document.documentElement;
			h.lang = fromElm.data;
			h.classList.add('lang-confirmed');
			break;
		}
		case 'share':
			navigator.share(fromElm.data).catch(() => {})
			break;
		case 'showModal':
			{
				const node = document.getElementById(fromElm.data);
				if (node) {
					if ('showModal' in node) {
						(node as HTMLDialogElement).showModal();
					} else {
						node.toggleAttribute('open', true);
					}
				}
			}
			break;
		case 'slideViewportInto':
			{
				const node = document.getElementById(fromElm.data);
				if (node) {
					requestAnimationFrame(() => {
						const options: ScrollIntoViewOptions = { block: 'center' };
						if (!matchMedia('(prefers-reduced-motion)').matches) {
							options.behavior = 'smooth';
						}
						node.scrollIntoView(options);
					});
				}
			}
			break;
	}
});
