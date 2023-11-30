import { Module } from '../Module.ts'
import { error } from '../errors.ts'

export interface ShaderProgramWithLocations {
	program: WebGLProgram,
	locations: {
		attributes: Record<string, number>,
		uniforms: Record<string, WebGLUniformLocation>
	}
}

export interface Framebuffer {
	glfb: WebGLFramebuffer,
	width: number,
	height: number
}

export interface Texture {
	gltx: WebGLTexture,
	width: number,
	height: number
}

type ShaderType =
	'bool' | 'int' | 'uint' | 'float' | 'double' |
	'vec2' | 'vec3' | 'vec4' |
	'mat2' | 'mat3' | 'mat4' | 'mat2x2' | 'mat2x3' | 'mat2x4' | 'mat3x2' |
	'mat3x3' | 'mat3x4' | 'mat4x2' | 'mat4x3' | 'mat4x4' |
	'sampler2D'

const FORMAT_MAPPINGS = {
	'RGB': 'RGB',
	'RGBA': 'RGBA',
	'LUMINANCE_ALPHA': 'LUMINANCE_ALPHA',
	'LUMINANCE': 'LUMINANCE',
	'ALPHA': 'ALPHA',
	'R8': 'RED',
	'R16F': 'RED',
	'R32F': 'RED',
	'R8UI': 'RED_INTEGER',
	'RG8': 'RG',
	'RG16F': 'RG',
	'RG32F': 'RG',
	'RG8UI': 'RG_INTEGER',
	'RGB8': 'RGB',
	'SRGB8': 'RGB',
	'RGB565': 'RGB',
	'R11F_G11F_B10F': 'RGB',
	'RGB9_E5': 'RGB',
	'RGB16F': 'RGB',
	'RGB32F': 'RGB',
	'RGB8UI': 'RGB_INTEGER',
	'RGBA8': 'RGBA',
	'SRGB8_ALPHA8': 'RGBA',
	'RGB5_A1': 'RGBA',
	'RGB10_A2': 'RGBA',
	'RGBA4': 'RGBA',
	'RGBA16F': 'RGBA',
	'RGBA32F': 'RGBA',
	'RGBA8UI': 'RGBA_INTEGER'
} as const

const POSSIBLE_FORMAT_CHOICES = {
	'RGB': ['UNSIGNED_BYTE', 'UNSIGNED_SHORT_5_6_5'],
	'RGBA': [
		'UNSIGNED_BYTE', 'UNSIGNED_SHORT_4_4_4_4', 'UNSIGNED_SHORT_5_5_5_1'
	],
	'LUMINANCE_ALPHA': ['UNSIGNED_BYTE'],
	'LUMINANCE': ['UNSIGNED_BYTE'],
	'ALPHA': ['UNSIGNED_BYTE'],
	'R8': ['UNSIGNED_BYTE'],
	'R16F': ['HALF_FLOAT', 'FLOAT'],
	'R32F': ['FLOAT'],
	'R8UI': ['UNSIGNED_BYTE'],
	'RG8': ['UNSIGNED_BYTE'],
	'RG16F': ['HALF_FLOAT', 'FLOAT'],
	'RG32F': ['FLOAT'],
	'RG8UI': ['UNSIGNED_BYTE'],
	'RGB8': ['UNSIGNED_BYTE'],
	'SRGB8': ['UNSIGNED_BYTE'],
	'RGB565': ['UNSIGNED_BYTE', 'UNSIGNED_SHORT_5_6_5'],
	'R11F_G11F_B10F': ['UNSIGNED_INT_10F_11F_11F_REV', 'HALF_FLOAT', 'FLOAT'],
	'RGB9_E5': ['HALF_FLOAT', 'FLOAT'],
	'RGB16F': ['HALF_FLOAT', 'FLOAT'],
	'RGB32F': ['FLOAT'],
	'RGB8UI': ['UNSIGNED_BYTE'],
	'RGBA8': ['UNSIGNED_BYTE'],
	'SRGB8_ALPHA8': ['UNSIGNED_BYTE'],
	'RGB5_A1': ['UNSIGNED_BYTE', 'UNSIGNED_SHORT_5_5_5_1'],
	'RGB10_A2': ['UNSIGNED_INT_2_10_10_10_REV'],
	'RGBA4': ['UNSIGNED_BYTE', 'UNSIGNED_SHORT_4_4_4_4'],
	'RGBA16F': ['HALF_FLOAT', 'FLOAT'],
	'RGBA32F': ['FLOAT'],
	'RGBA8UI': ['UNSIGNED_BYTE'],
} as const
type InnerFormatType = keyof typeof FORMAT_MAPPINGS

type TransformMatrix = Float32Array

export class Frame extends Module {
	public a = 123;
}
