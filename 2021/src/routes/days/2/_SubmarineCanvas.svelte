<script lang="ts">
	import * as THREE from 'three';
	import * as SC from 'svelte-cubed';
	import { onMount } from 'svelte';
	import { tweened } from 'svelte/motion';
	import type { GeneratorResult } from '$lib/2';

	export let size: number;
	export let genFn: () => Generator<GeneratorResult, void>;

	let gen = genFn();
	let submarine: THREE.Material;
	const depth = tweened(0);
	const horizontal = tweened(0);
	let done = false;

	let hLineGeometry: THREE.BufferGeometry;
	let dLineGeometry: THREE.BufferGeometry;
	$: {
		const hPoints = [
			new THREE.Vector3($horizontal / size, 0, 0),
			new THREE.Vector3($horizontal / size, 0, $depth / size)
		];
		hLineGeometry = new THREE.BufferGeometry().setFromPoints(hPoints);
		const dPoints = [
			new THREE.Vector3(0, 0, $depth / size),
			new THREE.Vector3($horizontal / size, 0, $depth / size)
		];
		dLineGeometry = new THREE.BufferGeometry().setFromPoints(dPoints);
	}

	SC.onFrame(() => {
		if (done) {
			return;
		}
		const res = gen.next();
		if (!res.done) {
			const val = res.value as GeneratorResult;
			horizontal.set(val.horizontal);
			depth.set(val.depth);
		} else {
			done = res.done;
		}
	});

	onMount(() => {
		const loader = new THREE.ImageBitmapLoader();
		loader.setOptions({ imageOrientation: 'none' });
		loader.load('/submarine.png', (imageBitmap) => {
			const texture = new THREE.CanvasTexture(imageBitmap);
			submarine = new THREE.MeshBasicMaterial({ map: texture, side: THREE.BackSide });
		});
	});
</script>

<div class="relative" style="width: 400px; height: 400px">
	<div class="absolute left-8 z-50 block" style={`top: ${20 + ($depth / (size / 2)) * 400}px;`}>
		depth <code>{Math.floor($depth)}</code>
	</div>
	<div
		class="absolute top-6 z-50 block"
		style={`left: ${20 + ($horizontal / (size / 2)) * 400}px;`}
	>
		position <code>{Math.floor($horizontal)}</code>
	</div>
	<SC.Canvas antialias>
		<SC.PerspectiveCamera
			position={[0, 1, 0]}
			zoom={0.78}
			viewOffset={{ fullHeight: 1, fullWidth: 1, height: 0.5, width: 0.5, x: 0.48, y: 0.48 }}
		/>
		<SC.Primitive object={new THREE.GridHelper(1)} position={[0, 0, 0]} />
		<SC.Primitive
			object={new THREE.ArrowHelper(
				new THREE.Vector3(1, 0, 0),
				new THREE.Vector3(0, 0, 0),
				0.5,
				undefined,
				0.05,
				0.03
			)}
		/>
		<SC.Primitive
			object={new THREE.ArrowHelper(
				new THREE.Vector3(0, 0, 1),
				new THREE.Vector3(0, 0, 0),
				0.5,
				undefined,
				0.05,
				0.03
			)}
		/>
		<SC.Primitive
			object={new THREE.Line(hLineGeometry, new THREE.LineBasicMaterial({ color: 'white' }))}
		/>
		<SC.Primitive
			object={new THREE.Line(dLineGeometry, new THREE.LineBasicMaterial({ color: 'white' }))}
		/>
		<SC.Mesh
			geometry={new THREE.PlaneGeometry(0.05, 0.05)}
			material={submarine}
			rotation={[Math.PI / 2, 0, 0]}
			position={[$horizontal / size, 0, $depth / size]}
		/>
	</SC.Canvas>
</div>
