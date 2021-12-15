import { page } from '$app/stores';
import { derived } from 'svelte/store';

export const activeDay = derived(page, ({ path }) => {
	if (!path.startsWith('/days')) {
		return null;
	}
	const splitPath = path.split('/');
	return Number(splitPath[splitPath.length - 1]);
});
