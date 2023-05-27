class ImageCell {
    dispose() {
      
    }
    
    constructor(parent, data) {
      let elt = document.createElement("div");
    
      elt.classList.add("frontend-object");
      elt.style.display = 'block';
      parent.element.appendChild(elt);  
      parent.element.classList.add('padding-fix');
  
      let img = document.createElement("img");
      
      img.src = data;
      elt.appendChild(img);  
      
      return this;
    }
  }
  
window.SupportedCells['image'] = {
  view: ImageCell
};

class FileOutputCell {
    dispose() {
      
    }
    
    constructor(parent, data) {
      new EditorView({
        doc: data,
        extensions: [
          EditorState.readOnly.of(true),
          syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
          editorCustomTheme
        ],
        parent: parent.element
      }); 
      
      return this;
    }
  }
  
  window.SupportedCells['fileprint'] = {
    view: FileOutputCell
  };
