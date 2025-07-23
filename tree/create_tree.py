import csv
import io

# Dati estratti dall'immagine CSV.
# In un caso reale, potresti leggere questi dati da un file .csv.
CSV_DATA = """name,id,parent_id,description
freshwater,1,1,1 freshwater
land cover,2,2,2 land cover
cloud,3,3,3 cloud
topography,4,4,4 topography
slope,5,1,1 slope of freshwater
slope,6,4,4 slope of terrestrial
t,7,1,1 temperature
yearly,8,3,3 yearly cloud cover
mn,9,7,7 mean temperature
max,10,7,7 maximum temperature
"""

def build_tree(data):
    """
    Costruisce una struttura ad albero (o foresta) dai dati forniti.
    """
    nodes = {}
    # Prima passata: crea un dizionario di nodi e aggiungi una lista 'children' vuota.
    for row in data:
        row['children'] = []
        nodes[int(row['id'])] = row

    root_nodes = []
    # Seconda passata: popola le liste 'children' e identifica i nodi radice.
    for row in data:
        node_id = int(row['id'])
        parent_id = int(row['parent_id'])
        
        # Se un nodo ha un genitore diverso da se stesso, lo aggiunge alla lista dei figli del genitore.
        if parent_id != node_id and parent_id in nodes:
            parent_node = nodes[parent_id]
            parent_node['children'].append(nodes[node_id])
        # Altrimenti, è un nodo radice (o un orfano).
        else:
            root_nodes.append(nodes[node_id])
            
    return root_nodes

def generate_html_list(nodes):
    """
    Genera ricorsivamente una lista HTML (<ul><li>...</li></ul>) dalla struttura ad albero.
    """
    if not nodes:
        return ""
        
    html = "<ul>\n"
    for node in sorted(nodes, key=lambda x: int(x['id'])):
        # Aggiunge una classe 'parent' se il nodo ha figli, per lo stile e l'interattività.
        css_class = "class='parent'" if node['children'] else ""
        html += f"<li {css_class}><span>{node['name']}</span> <i>(id: {node['id']}, desc: {node['description']})</i>"
        html += generate_html_list(node['children'])
        html += "</li>\n"
    html += "</ul>\n"
    return html

def create_html_file(tree_html, filename="albero.html"):
    """
    Crea il file HTML finale incorporando la struttura ad albero, CSS e JavaScript.
    """
    html_content = f"""
<!DOCTYPE html>
<html lang="it">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Albero Gerarchico</title>
    <style>
        body {{
            font-family: sans-serif;
            margin: 2em;
        }}
        /* Rimuove i punti elenco e imposta il padding */
        ul {{
            list-style-type: none;
            padding-left: 20px;
        }}
        /* Stile base per ogni elemento della lista */
        li {{
            padding: 4px;
            position: relative;
        }}
        /* Stile per i nodi che hanno figli */
        li.parent > span {{
            cursor: pointer;
            font-weight: bold;
        }}
        /* Aggiunge un indicatore (triangolo) per i nodi espandibili/comprimibili */
        li.parent > span::before {{
            content: '▼';
            display: inline-block;
            width: 1em;
            color: #666;
            transition: transform 0.2s;
        }}
        /* Ruota il triangolo quando il nodo è compresso */
        li.parent.collapsed > span::before {{
            transform: rotate(-90deg);
        }}
        /* Nasconde i figli di un nodo compresso */
        li.parent.collapsed > ul {{
            display: none;
        }}
        /* Aggiunge le linee di collegamento per la gerarchia */
        li::before, li::after {{
            content: '';
            position: absolute;
            left: -12px;
        }}
        li::before {{
            border-top: 1px solid #ccc;
            top: 15px;
            width: 10px;
            height: 0;
        }}
        li::after {{
            border-left: 1px solid #ccc;
            height: 100%;
            width: 0px;
            top: -5px;
        }}
        /* Rimuove la linea verticale per l'ultimo elemento di una lista */
        li:last-child::after {{
            height: 20px;
        }}
    </style>
</head>
<body>
    <h1>Albero Gerarchico dai Dati CSV</h1>
    <div id="tree-container">
        {tree_html}
    </div>

    <script>
        // Aggiunge la funzionalità di click per espandere/comprimere i nodi
        document.querySelectorAll('li.parent > span').forEach(span => {{
            span.addEventListener('click', function() {{
                this.parentElement.classList.toggle('collapsed');
            }});
        }});
    </script>
</body>
</html>
    """
    with open(filename, "w", encoding="utf-8") as f:
        f.write(html_content)
    print(f"File '{filename}' creato con successo.")

# --- Esecuzione Principale ---
if __name__ == "__main__":
    # Usa io.StringIO per trattare la stringa CSV come un file.
    # In un caso reale, useresti: with open('tuo_file.csv', 'r') as f:
    csv_file = io.StringIO(CSV_DATA)
    
    # Legge i dati CSV in una lista di dizionari.
    reader = csv.DictReader(csv_file)
    data = list(reader)
    
    # 1. Costruisce la struttura dati ad albero.
    tree_data = build_tree(data)
    
    # 2. Genera il markup HTML per l'albero.
    html_for_tree = generate_html_list(tree_data)
    
    # 3. Crea il file HTML completo.
    create_html_file(html_for_tree)