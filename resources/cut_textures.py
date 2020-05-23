import os
from PIL import Image

src_dir = "res\\original"
out_dir = "res\\cut"


def cut_image(img: Image):
    left, upper, right, lower = img.getbbox()
    return img.crop((0, upper, img.width, lower))


for curr_dir, dirs, files in os.walk(src_dir):
    if len(dirs) != 0:
        for directory in dirs:
            out_path = os.path.join(out_dir, directory)
            os.makedirs(out_path, exist_ok=True)
    else:
        dir_name = os.path.basename(curr_dir)
        out_path = os.path.join(out_dir, dir_name)
        for filename in files:
            image_path = os.path.join(curr_dir, filename)
            save_path = os.path.join(out_path, filename)

            image = Image.open(image_path)
            new_image = cut_image(image)
            new_image.save(save_path)