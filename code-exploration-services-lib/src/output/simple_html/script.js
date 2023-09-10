function change_theme(new_theme) {
    for (const i of document.getElementsByClassName("change-theme")) {
        i.className = `change-theme ${new_theme}`;
    }
}

const highlights = new Map();

function register_highlight(location) {
    for (const i of highlights.keys()) {
        clearTimeout(highlights.get(i));
        for (const el of document.getElementsByClassName(i)) {
            el.classList.remove("highlighted")
        }
    }
    highlights.clear()

    for (const i of document.getElementsByClassName(location)) {
        i.classList.add("highlighted")
    }

    const cancel = setTimeout(() => {
        for (const i of document.getElementsByClassName(location)) {
            i.classList.remove("highlighted")
        }

        highlights.delete(location);
    }, 2000);

    highlights.set(location, cancel);
}

function go_to(location) {
    register_highlight(location);

    let el;
    for (const i of document.getElementsByClassName(location)) {
        if (!i.classList.contains("outline")) {
            el = i;
            break;
        }
    }

    el.scrollIntoView({
        behavior: "smooth",
        block: "center",
        inline: "nearest"
    })
}

document.getElementById("change-theme").onchange = function () {
    change_theme(this.value)
};

document.onreadystatechange = () => {
    change_theme(document.getElementById("change-theme").value);

    for (const i of document.getElementsByClassName("outline-header")) {
        i.onclick = () => {
            go_to(i.dataset.gotoClass);
        }
    }

    for (const i of document.getElementsByClassName("reference-item")) {
        i.onclick = (evt) => {
            evt.stopPropagation()
            for (const i of document.getElementsByClassName("reference-popup")) {
                i.style.display = "none";
            }

            go_to(i.dataset.gotoClass);
        }
    }


    document.onclick = () => {
        for (const i of document.getElementsByClassName("reference-popup")) {
            i.style.display = "none";
        }
    };

    for (const i of document.getElementsByClassName("token")) {
        const reference_children = i.getElementsByClassName("goto-reference-instantly")
        if (reference_children.length === 1) {
            i.onclick = () => {
                const child = reference_children[0];
                go_to(child.dataset.gotoClass);
            };
        }

        const popup_children = i.getElementsByClassName("reference-popup")
        if (popup_children.length === 1) {
            i.onclick = (e) => {
                e.stopPropagation();

                for (const i of document.getElementsByClassName("reference-popup")) {
                    i.style.display = "none";
                }

                const popup = popup_children[0];
                popup.style.display = "flex";
            };
        }
    }
}
