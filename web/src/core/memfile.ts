export type MemFileKind = 'neander' | 'ahmes' | 'ramses' | 'cesar';

interface MemFileSpec {
  signature: string;
  size: number;
}

const SPECS: Readonly<Record<MemFileKind, MemFileSpec>> = {
  neander: { signature: 'NDR', size: 256 },
  ahmes: { signature: 'NDR', size: 256 },
  ramses: { signature: 'RMS', size: 256 },
  cesar: { signature: 'C16', size: 65536 },
};

const HEADER_PREFIX = 0x03;
const HEADER_LENGTH = 4;

const encodeHeader = (signature: string): Uint8Array => {
  const out = new Uint8Array(HEADER_LENGTH);
  out[0] = HEADER_PREFIX;
  out[1] = signature.charCodeAt(0);
  out[2] = signature.charCodeAt(1);
  out[3] = signature.charCodeAt(2);
  return out;
};

const decodeSignature = (file: Uint8Array): string =>
  String.fromCharCode(file[1] ?? 0, file[2] ?? 0, file[3] ?? 0);

export function writeMemFile(kind: MemFileKind, data: Uint8Array): Uint8Array {
  const spec = SPECS[kind];
  if (data.length !== spec.size) {
    throw new Error(
      `Memória inválida para ${kind}: esperado ${spec.size} bytes, obtido ${data.length}`,
    );
  }
  const out = new Uint8Array(HEADER_LENGTH + spec.size);
  out.set(encodeHeader(spec.signature), 0);
  out.set(data, HEADER_LENGTH);
  return out;
}

export function readMemFile(kind: MemFileKind, file: Uint8Array): Uint8Array {
  const spec = SPECS[kind];
  const expectedLength = HEADER_LENGTH + spec.size;
  if (file.length !== expectedLength) {
    throw new Error(
      `Arquivo .mem inválido: esperado ${expectedLength} bytes, obtido ${file.length}`,
    );
  }
  if (file[0] !== HEADER_PREFIX) {
    throw new Error(`Arquivo .mem inválido: prefixo esperado 0x03, obtido 0x${(file[0] ?? 0).toString(16)}`);
  }
  const signature = decodeSignature(file);
  if (signature !== spec.signature) {
    throw new Error(
      `Arquivo .mem inválido: assinatura esperada "${spec.signature}", obtida "${signature}"`,
    );
  }
  return file.slice(HEADER_LENGTH);
}

export function memFileSignature(kind: MemFileKind): string {
  return SPECS[kind].signature;
}

export function memFileSize(kind: MemFileKind): number {
  return SPECS[kind].size;
}
