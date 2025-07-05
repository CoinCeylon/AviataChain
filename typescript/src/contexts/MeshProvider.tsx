import { ReactNode } from 'react';
import { MeshProvider } from '@meshsdk/react';

type Props = { children: ReactNode };

export default function CustomMeshProvider({ children }: Props) {
  return <MeshProvider>{children}</MeshProvider>;
}
