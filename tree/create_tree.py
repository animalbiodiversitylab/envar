import pandas as pd
import json
from pathlib import Path

def generate_html_tree(csv_file_path, output_file_path="tree_navigator.html"):
    """
    Genera un albero HTML navigabile da un file CSV con struttura gerarchica.
    
    Args:
        csv_file_path (str): Percorso del file CSV di input
        output_file_path (str): Percorso del file HTML di output
    """
    
    # Leggi il CSV
    try:
        df = pd.read_csv(csv_file_path)
        print(f"CSV caricato con successo: {len(df)} righe")
    except Exception as e:
        print(f"Errore nel caricamento del CSV: {e}")
        return
    
    # Verifica che le colonne necessarie esistano
    required_columns = ['name', 'id', 'parent_id']
    missing_columns = [col for col in required_columns if col not in df.columns]
    if missing_columns:
        print(f"Colonne mancanti nel CSV: {missing_columns}")
        return
    
    # Gestisci colonne opzionali
    if 'description' not in df.columns:
        df['description'] = ''
    if 'link_download' not in df.columns:
        df['link_download'] = ''
    
    # Usa 'incolla_valori' come fallback per link_download se disponibile
    if 'incolla_valori' in df.columns:
        df['link_download'] = df['link_download'].fillna('').astype(str)
        df['incolla_valori'] = df['incolla_valori'].fillna('').astype(str)
        # Usa incolla_valori se link_download è vuoto
        mask = (df['link_download'] == '') | (df['link_download'] == 'nan')
        df.loc[mask, 'link_download'] = df.loc[mask, 'incolla_valori']
    
    # Pulisci i dati
    df['parent_id'] = df['parent_id'].fillna('')
    df['description'] = df['description'].fillna('')
    df['link_download'] = df['link_download'].fillna('')
    
    # Fix per cicli self-referencing (parent_id == id)
    cycle_mask = df['id'] == df['parent_id']
    if cycle_mask.any():
        cycle_nodes = df[cycle_mask]['name'].tolist()
        print(f"⚠️  Fix automatico per nodi con cicli: {cycle_nodes}")
        df.loc[cycle_mask, 'parent_id'] = ''  # Trasforma in root nodes
    
    # Converti ID in string per evitare problemi di tipo
    df['id'] = df['id'].astype(str)
    df['parent_id'] = df['parent_id'].astype(str)
    
    # Costruisci la struttura ad albero
    def build_tree_structure():
        # Crea un dizionario per accesso rapido per ID
        nodes = {}
        for _, row in df.iterrows():
            nodes[row['id']] = {
                'id': row['id'],
                'name': row['name'],
                'description': row['description'],
                'link_download': row['link_download'],
                'parent_id': row['parent_id'],
                'children': []
            }
        
        # Trova i nodi root e costruisci la gerarchia
        root_nodes = []
        for node_id, node in nodes.items():
            parent_id = node['parent_id']
            if parent_id == '' or parent_id not in nodes:
                # Nodo root
                root_nodes.append(node)
            else:
                # Aggiungi come figlio del parent
                nodes[parent_id]['children'].append(node)
        
        return root_nodes
    
    tree_data = build_tree_structure()
    
    # Template HTML
    html_template = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ENVAR Navigator</title>
    <style>
        :root {
            --primary-color: #9aa0d6; /* Purple/blue from logo */
            --secondary-color: #a3d45c; /* Green from logo border */
            --accent-color: #3da3c2; /* Blue from R circle */
            --text-color: #333;
            --background-color: #f8f9fa;
            --header-text-color: #ffffff;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
            line-height: 1.6;
            color: var(--text-color);
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background-color: var(--background-color);
        }
        
        .header {
            background: linear-gradient(135deg, var(--primary-color) 0%, #5d6094 100%);
            color: var(--header-text-color);
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
            text-align: center;
            border: 4px solid var(--secondary-color);
        }
        
        .logo-container {
            display: flex;
            justify-content: center;
            align-items: center;
            margin-bottom: 15px;
        }
        
        .logo {
            height: 80px;
            width: auto;
        }
        
        .header h1 {
            margin: 0;
            font-size: 2.5em;
        }
        
        .header p {
            margin: 10px 0 0 0;
            opacity: 0.9;
        }
        
        .tree-container {
            background: white;
            border-radius: 10px;
            padding: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        
        .tree-node {
            margin-left: 20px;
            border-left: 2px solid #e9ecef;
            padding-left: 15px;
            margin-bottom: 10px;
        }
        
        .tree-node.root {
            margin-left: 0;
            border-left: none;
            padding-left: 0;
        }
        
        .node-header {
            display: flex;
            align-items: center;
            padding: 12px;
            background-color: #f8f9fa;
            border-radius: 8px;
            margin-bottom: 10px;
            cursor: pointer;
            transition: all 0.3s ease;
            border: 1px solid #dee2e6;
        }
        
        .node-header:hover {
            background-color: #e9ecef;
            transform: translateX(5px);
        }
        
        .node-header.expanded {
            background-color: #e3f2fd;
            border-color: var(--accent-color);
        }
        
        .expand-icon {
            margin-right: 10px;
            font-size: 12px;
            width: 20px;
            text-align: center;
            color: #666;
            transition: transform 0.3s ease;
        }
        
        .expand-icon.expanded {
            transform: rotate(90deg);
        }
        
        .node-name {
            font-weight: 600;
            color: #2c3e50;
            flex-grow: 1;
        }
        
        .node-info {
            padding: 15px;
            margin: 10px 0;
            background-color: #fff;
            border-radius: 8px;
            border: 1px solid #dee2e6;
            display: none;
        }
        
        .node-info.visible {
            display: block;
            animation: fadeIn 0.3s ease;
        }
        
        .node-description {
            color: #6c757d;
            margin-bottom: 15px;
            line-height: 1.5;
        }
        
        .download-link {
            display: inline-flex;
            align-items: center;
            padding: 8px 16px;
            background-color: var(--accent-color);
            color: white;
            text-decoration: none;
            border-radius: 5px;
            font-size: 14px;
            transition: background-color 0.3s ease;
        }
        
        .download-link:hover {
            background-color: #2d7a92;
        }
        
        .download-link:before {
            content: "⬇ ";
            margin-right: 5px;
        }
        
        .children {
            display: none;
            margin-top: 10px;
        }
        
        .children.visible {
            display: block;
            animation: slideDown 0.3s ease;
        }
        
        .search-container {
            margin-bottom: 30px;
        }
        
        .search-input {
            width: 100%;
            padding: 15px;
            font-size: 16px;
            border: 2px solid #dee2e6;
            border-radius: 8px;
            box-sizing: border-box;
        }
        
        .search-input:focus {
            outline: none;
            border-color: var(--primary-color);
        }
        
        .stats {
            background-color: rgba(163, 212, 92, 0.2);
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
            text-align: center;
            border: 1px solid var(--secondary-color);
        }
        
        .highlight {
            background-color: #fff3cd;
            padding: 2px 4px;
            border-radius: 3px;
        }
        
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(-10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        
        @keyframes slideDown {
            from { opacity: 0; transform: scaleY(0); }
            to { opacity: 1; transform: scaleY(1); }
        }
        
        @media (max-width: 768px) {
            body {
                padding: 10px;
            }
            
            .header h1 {
                font-size: 2em;
            }
            
            .tree-container {
                padding: 20px;
            }
            
            .tree-node {
                margin-left: 10px;
                padding-left: 10px;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="logo-container">
            <img src="../assets/logo.svg" alt="ENVAR Logo" class="logo">
        </div>
        <h1>ENVAR Navigator</h1>
        <p>Explore the hierarchical structure of our processed variables from each datasets</p>
    </div>
    
    <div class="search-container">
        <input type="text" class="search-input" placeholder="Search variables by name or description..." id="searchInput">
    </div>
    
    <div class="stats" id="stats">
        Caricamento statistiche...
    </div>
    
    <div class="tree-container">
        <div id="treeRoot"></div>
    </div>

    <script>
        const treeData = TREE_DATA_PLACEHOLDER;
        
        let allNodes = [];
        let expandedNodes = new Set();
        
        function flattenTree(nodes, depth = 0) {
            let flattened = [];
            nodes.forEach(node => {
                flattened.push({...node, depth});
                if (node.children && node.children.length > 0) {
                    flattened = flattened.concat(flattenTree(node.children, depth + 1));
                }
            });
            return flattened;
        }
        
        function updateStats() {
            allNodes = flattenTree(treeData);
            const totalNodes = allNodes.length;
            const leafNodes = allNodes.filter(node => !node.children || node.children.length === 0).length;
            const maxDepth = Math.max(...allNodes.map(node => node.depth));
            
            document.getElementById('stats').innerHTML = `
                <strong>${totalNodes}</strong> total variables | 
                <strong>${leafNodes}</strong> leaf variables | 
                Maximum depth: <strong>${maxDepth + 1}</strong>
            `;
        }
        
        function createNodeElement(node, isRoot = false) {
            const hasChildren = node.children && node.children.length > 0;
            const nodeId = `node-${node.id}`;
            
            const nodeDiv = document.createElement('div');
            nodeDiv.className = `tree-node ${isRoot ? 'root' : ''}`;
            nodeDiv.id = nodeId;  // Aggiungi l'ID corretto
            nodeDiv.setAttribute('data-node-id', node.id);
            
            const headerDiv = document.createElement('div');
            headerDiv.className = 'node-header';
            headerDiv.onclick = () => toggleNode(nodeId, hasChildren);
            
            const expandIcon = document.createElement('span');
            expandIcon.className = 'expand-icon';
            expandIcon.innerHTML = hasChildren ? '▶' : '•';
            
            const nameSpan = document.createElement('span');
            nameSpan.className = 'node-name';
            nameSpan.textContent = node.name;
            
            headerDiv.appendChild(expandIcon);
            headerDiv.appendChild(nameSpan);
            
            nodeDiv.appendChild(headerDiv);
            
            // Info del nodo
            const infoDiv = document.createElement('div');
            infoDiv.className = 'node-info';
            infoDiv.id = `info-${node.id}`;
            
            let infoContent = '';
            if (node.description) {
                infoContent += `<div class="node-description">${node.description}</div>`;
            }
            if (node.link_download) {
                infoContent += `<a href="${node.link_download}" class="download-link" target="_blank">Download</a>`;
            }
            
            if (infoContent) {
                infoDiv.innerHTML = infoContent;
                nodeDiv.appendChild(infoDiv);
            }
            
            // Figli
            if (hasChildren) {
                const childrenDiv = document.createElement('div');
                childrenDiv.className = 'children';
                childrenDiv.id = `children-${node.id}`;
                
                node.children.forEach(child => {
                    childrenDiv.appendChild(createNodeElement(child));
                });
                
                nodeDiv.appendChild(childrenDiv);
            }
            
            return nodeDiv;
        }
        
        function toggleNode(nodeId, hasChildren) {
            const nodeElement = document.getElementById(nodeId);  // Usa nodeId direttamente
            const header = nodeElement.querySelector('.node-header');
            const info = document.getElementById(`info-${nodeId.replace('node-', '')}`);
            const children = document.getElementById(`children-${nodeId.replace('node-', '')}`);
            const expandIcon = header.querySelector('.expand-icon');
            
            const isExpanded = expandedNodes.has(nodeId);
            
            if (isExpanded) {
                expandedNodes.delete(nodeId);
                header.classList.remove('expanded');
                if (info) info.classList.remove('visible');
                if (children) children.classList.remove('visible');
                if (hasChildren) expandIcon.classList.remove('expanded');
            } else {
                expandedNodes.add(nodeId);
                header.classList.add('expanded');
                if (info) info.classList.add('visible');
                if (children) children.classList.add('visible');
                if (hasChildren) expandIcon.classList.add('expanded');
            }
        }
        
        function searchTree(query) {
            const searchTerm = query.toLowerCase();
            const nodes = document.querySelectorAll('.tree-node');
            
            nodes.forEach(node => {
                const name = node.querySelector('.node-name').textContent.toLowerCase();
                const description = node.querySelector('.node-description');
                const descText = description ? description.textContent.toLowerCase() : '';
                
                const matches = name.includes(searchTerm) || descText.includes(searchTerm);
                
                if (query === '') {
                    node.style.display = '';
                    // Rimuovi highlights
                    node.querySelectorAll('.highlight').forEach(el => {
                        el.outerHTML = el.innerHTML;
                    });
                } else if (matches) {
                    node.style.display = '';
                    // Aggiungi highlight
                    highlightText(node.querySelector('.node-name'), searchTerm);
                    if (description) highlightText(description, searchTerm);
                    
                    // Espandi i parent
                    let parent = node.parentElement;
                    while (parent && parent.classList.contains('children')) {
                        const parentNode = parent.parentElement;
                        const parentId = `node-${parentNode.getAttribute('data-node-id')}`;
                        if (!expandedNodes.has(parentId)) {
                            toggleNode(parentId, true);
                        }
                        parent = parentNode.parentElement;
                    }
                } else {
                    node.style.display = 'none';
                }
            });
        }
        
        function highlightText(element, searchTerm) {
            if (!searchTerm) return;
            
            const text = element.textContent;
            const regex = new RegExp(`(${searchTerm})`, 'gi');
            element.innerHTML = text.replace(regex, '<span class="highlight">$1</span>');
        }
        
        // Inizializzazione
        document.addEventListener('DOMContentLoaded', function() {
            const treeRoot = document.getElementById('treeRoot');
            
            treeData.forEach(node => {
                treeRoot.appendChild(createNodeElement(node, true));
            });
            
            updateStats();
            
            // Search functionality
            const searchInput = document.getElementById('searchInput');
            searchInput.addEventListener('input', (e) => {
                searchTree(e.target.value);
            });
        });
    </script>
</body>
</html>
    """
    
    # Sostituisci i dati nell'HTML
    tree_json = json.dumps(tree_data, ensure_ascii=False, indent=2)
    final_html = html_template.replace('TREE_DATA_PLACEHOLDER', tree_json)
    
    # Scrivi il file HTML
    try:
        with open(output_file_path, 'w', encoding='utf-8') as f:
            f.write(final_html)
        print(f"Albero HTML generato con successo: {output_file_path}")
        print(f"Puoi aprire il file con un browser o caricarlo su GitHub.")
        print(f"Nodi processati: {len(df)} | Nodi root: {len(tree_data)}")
    except Exception as e:
        print(f"Errore nella scrittura del file HTML: {e}")

# Esempio di utilizzo
if __name__ == "__main__":
    # Sostituisci con il percorso del tuo file CSV
    csv_file = "Sources.csv"  # Il tuo file
    
    # Per testare con i dati che hai fornito, puoi anche usarlo direttamente:
    # Salva il contenuto che hai incollato in un file chiamato "paste.txt"
    # oppure cambia csv_file con il nome del tuo file CSV
    
    # Genera l'albero HTML
    generate_html_tree(csv_file, "navigatore_variabili.html")
    
    print("\n🌳 Generazione completata!")
    print("📁 Apri 'navigatore_variabili.html' nel browser per vedere l'albero")
    print("🔍 Usa la barra di ricerca per trovare rapidamente le variabili")
    print("📤 Carica il file HTML su GitHub per condividerlo con altri utenti")
    print("\n💡 Suggerimenti:")
    print("- I nodi con cicli (parent_id == id) sono stati automaticamente corretti")
    print("- Se presente, 'incolla_valori' viene usato come link_download di fallback")
    print("- L'albero mostra: chelsa > GLOBAL > annual/climatologies/daily/etc.")