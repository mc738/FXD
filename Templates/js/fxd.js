const index = document.getElementById('index');

toggleIndex = () => {
    if (index.classList.contains("responsive"))
    {
        index.classList.remove("responsive")
    }
    else
    {
        index.classList.add("responsive")
    }

}