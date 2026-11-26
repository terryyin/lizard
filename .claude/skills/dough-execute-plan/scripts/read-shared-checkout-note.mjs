import { readFile } from "node:fs/promises";
import {
  parseSharedCheckoutNote,
  sharedCheckoutNoteOnReadFailure,
  sharedCheckoutNotePathBeside,
} from "./shared-checkout-note.mjs";

export async function readSharedCheckoutNote(landingContext) {
  try {
    return parseSharedCheckoutNote(
      await readFile(sharedCheckoutNotePathBeside(landingContext), "utf8"),
    );
  } catch (error) {
    return sharedCheckoutNoteOnReadFailure(error);
  }
}
