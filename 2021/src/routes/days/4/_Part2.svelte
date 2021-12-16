<script lang="ts">
	import { Field, part2 } from '$lib/4';

	export let fields: Field[];
	export let numbers: number[];

	let gen = part2(numbers, fields);
	let done = false;

	const handleClick = () => {
		if (!done) {
			const res = gen.next();
			done = res.done;
			const [n, fs] = res.value as [number, { field: Field; count: number }[]];
			drawn = [...drawn, n];
			localFields = fs;
			if (done) {
				result = (res.value as [number, { field: Field; count: number }[], number])[2];
			}
		}
	};
</script>
