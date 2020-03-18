import React from 'react';

interface TableProps {
    compact?: boolean
}

export const LightweightTable: React.FC<TableProps> = ({ children, compact }) => {
    return <table role='grid' className={'pf-c-table pf-m-grid-md' + (compact ? ' pf-m-compact' : '')}>{children}</table>
}


export interface Header {
    content: React.ReactNode
    width?: number | string
}

interface HeaderProps {
    headers: (string | Header)[]
}

export function LightweightTableHeader({ headers }: HeaderProps) {
    return <thead><tr>
        {headers.map((h, i) => {
            const header = typeof h === 'string' ? { content: h } : h as Header
            const className = header.width ? 'pf-m-width-' + header.width : ''
            return <th scope='col' key={i} className={className}>{header.content}</th>
        })}
    </tr></thead>
}