import type { Load } from '@sveltejs/kit';

export const load: Load = async ({ page, fetch }) => {
	const splitPath = page.path.split('/');
	const day = Number(splitPath[splitPath.length - 1]);
	const url = `/input/${day}.txt`;
	const res = await fetch(url);

	if (res.ok) {
		return {
			props: {
				input: await res.text()
			}
		};
	}

	return {
		status: res.status,
		error: new Error(`Could not load ${url}`)
	};
};
