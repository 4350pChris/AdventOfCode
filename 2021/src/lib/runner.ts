import { onDestroy } from 'svelte';
import { writable, type Writable } from 'svelte/store';

export function useRunner(callback: () => void): {
	run: () => void;
	stop: () => void;
	running: Writable<boolean>;
} {
	let runner: number;
	const running = writable<boolean>();

	onDestroy(stop);

	function run() {
		runner = window.setInterval(callback, 1000);
		running.set(true);
	}

	function stop() {
		if (runner) {
			clearInterval(runner);
			runner = undefined;
			running.set(false);
		}
	}

	return { run, stop, running };
}
